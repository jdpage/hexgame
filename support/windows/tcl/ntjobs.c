// NT Job Object API for Tcl
// Copyright (C) 2018  Jonathan David Page <jonathan@sleepingcyb.org>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#define _WIN32_WINNT 0x0500
#define UNICODE
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <tcl.h>
#include <tclOO.h>

#if defined(ntjobs_EXPORTS)
#define NTJOBS_EXPORT extern __declspec(dllexport)
#else
#define NTJOBS_EXPORT
#endif

#ifdef __GNUC__
#define UNUSED(x) UNUSED_ ## x __attribute__((__unused__))
#else
#define UNUSED(x) UNUSED_ ## x
#endif

#define TCLTRY(expr) do {                       \
    if ((expr) != TCL_OK) {                     \
      return TCL_ERROR;                         \
    }                                           \
  } while (0)

static void Ntjobs_DeleteJobMetadata(ClientData metadata)
{
  CloseHandle((HANDLE) metadata);
}

static int Ntjobs_CloneJobMetadata(
  Tcl_Interp *interp,
  ClientData UNUSED(srcMetadata),
  ClientData *UNUSED(dstMetadataPtr))
{
  Tcl_SetResult(interp, "nt::job objects cannot be cloned", TCL_STATIC);
  return TCL_ERROR;
}

static Tcl_ObjectMetadataType Ntjobs_JobMetadata = {
  .version = TCL_OO_METADATA_VERSION_CURRENT,
  .name = "Ntjobs_Metadata",
  .deleteProc = &Ntjobs_DeleteJobMetadata,
  .cloneProc = &Ntjobs_CloneJobMetadata,
};

static int Ntjobs_ContextToHandle(
  Tcl_ObjectContext context,
  HANDLE *handle)
{
  Tcl_Object self;
  if ((self = Tcl_ObjectContextObject(context)) == NULL) {
    return TCL_ERROR;
  }

  *handle = Tcl_ObjectGetMetadata(self, &Ntjobs_JobMetadata);
  return TCL_OK;
}

static int Ntjobs_FormatWin32Error(Tcl_Interp *interp, DWORD error)
{
  wchar_t *msg;
  Tcl_DString mbmsg;

  // Fetch the message
  FormatMessage(
    FORMAT_MESSAGE_ALLOCATE_BUFFER
    | FORMAT_MESSAGE_FROM_SYSTEM
    | FORMAT_MESSAGE_IGNORE_INSERTS,
    NULL, error, 0, (wchar_t *) &msg, 0, NULL);

  // Convert to UTF-8
  Tcl_DStringInit(&mbmsg);
  Tcl_DStringSetLength(
    &mbmsg, WideCharToMultiByte(CP_UTF8, 0, msg, -1, NULL, 0, NULL, NULL));
  WideCharToMultiByte(
    CP_UTF8, 0, msg, -1,
    Tcl_DStringValue(&mbmsg), Tcl_DStringLength(&mbmsg),
    NULL, NULL);
  LocalFree(msg);

  // Move into interp
  Tcl_DStringResult(interp, &mbmsg);
  Tcl_DStringFree(&mbmsg);
  return TCL_ERROR;
}

static int Ntjobs_JobConstructor(
  ClientData UNUSED(clientData),
  Tcl_Interp *interp,
  Tcl_ObjectContext objectContext,
  int objc,
  Tcl_Obj *const *objv)
{
  Tcl_Object self;
  HANDLE job;

  if ((self = Tcl_ObjectContextObject(objectContext)) == NULL) {
    return TCL_ERROR;
  }

  if (strcmp("new", Tcl_GetString(objv[1])) == 0) {
    if (objc != 2) {
      Tcl_WrongNumArgs(interp, 1, objv, "new");
      return TCL_ERROR;
    }
  } else if (strcmp("create", Tcl_GetString(objv[1])) == 0) {
    if (objc != 3) {
      Tcl_WrongNumArgs(interp, 1, objv, "create name");
      return TCL_ERROR;
    }
  } else {
    if (objc != 1) {
      Tcl_WrongNumArgs(interp, 1, objv, "");
      return TCL_ERROR;
    }
  }

  if ((job = CreateJobObject(NULL, NULL)) == NULL) {
    return Ntjobs_FormatWin32Error(interp, GetLastError());
  }

  Tcl_ObjectSetMetadata(self, &Ntjobs_JobMetadata, job);
  return TCL_OK;
}

static int Ntjobs_JobMethodLimit(
  ClientData UNUSED(clientData),
  Tcl_Interp *interp,
  Tcl_ObjectContext objectContext,
  int objc,
  Tcl_Obj *const *objv)
{
  HANDLE self;
  JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits;
  TCLTRY(Ntjobs_ContextToHandle(objectContext, &self));

  if (objc < 4 || (objc % 2) != 0) {
    Tcl_WrongNumArgs(interp, 1, objv, "limit -option value ?...?");
    return TCL_ERROR;
  }

  if (!QueryInformationJobObject(
        self, JobObjectExtendedLimitInformation,
        &limits, sizeof(JOBOBJECT_EXTENDED_LIMIT_INFORMATION),
        NULL))
  {
    return Ntjobs_FormatWin32Error(interp, GetLastError());
  }

  for (int k = 2; k < objc; k += 2) {
    char *opt = Tcl_GetString(objv[k]);
    if (strcmp("-killonjobclose", opt) == 0) {
      int flag;
      TCLTRY(Tcl_GetIntFromObj(interp, objv[k+1], &flag));
      if (flag) {
        limits.BasicLimitInformation.LimitFlags
          |= JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
      } else {
        limits.BasicLimitInformation.LimitFlags
          &= ~((DWORD) JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE);
      }
    } else {
      Tcl_SetObjResult(interp, Tcl_ObjPrintf("unknown option \"%s\"", opt));
      return TCL_ERROR;
    }
  }

  if (!SetInformationJobObject(
        self, JobObjectExtendedLimitInformation,
        &limits, sizeof(JOBOBJECT_EXTENDED_LIMIT_INFORMATION)))
  {
    return Ntjobs_FormatWin32Error(interp, GetLastError());
  }

  return TCL_OK;
}

static int Ntjobs_JobMethodAssign(
  ClientData UNUSED(clientData),
  Tcl_Interp *interp,
  Tcl_ObjectContext objectContext,
  int objc,
  Tcl_Obj *const *objv)
{
  HANDLE self;
  TCLTRY(Ntjobs_ContextToHandle(objectContext, &self));

  if (objc < 3) {
    Tcl_WrongNumArgs(interp, 1, objv, "assign pid ?pid ...?");
    return TCL_ERROR;
  }

  for (int k = 2; k < objc; k++) {
    int pid, err = TCL_OK;
    HANDLE proc;
    TCLTRY(Tcl_GetIntFromObj(interp, objv[k], &pid));
    if (pid < 1) {
      Tcl_SetObjResult(interp, Tcl_ObjPrintf("invalid pid %d", pid));
      return TCL_ERROR;
    }

    if ((proc = OpenProcess(
           PROCESS_SET_QUOTA | PROCESS_TERMINATE, 0, (DWORD) pid)) == NULL) {
      return Ntjobs_FormatWin32Error(interp, GetLastError());
    }

    if (!AssignProcessToJobObject(self, proc)) {
      err = Ntjobs_FormatWin32Error(interp, GetLastError());
    }

    CloseHandle(proc);
    if (err != TCL_OK) { return err; }
  }

  return TCL_OK;
}

static Tcl_MethodType Ntjobs_JobMethods[] = {
  { TCL_OO_METHOD_VERSION_CURRENT, NULL, Ntjobs_JobConstructor, NULL, NULL },
  { TCL_OO_METHOD_VERSION_CURRENT, "limits", Ntjobs_JobMethodLimit, NULL, NULL },
  { TCL_OO_METHOD_VERSION_CURRENT, "assign", Ntjobs_JobMethodAssign, NULL, NULL },
  { -1, NULL, NULL, NULL, NULL },
};

static int Ntjobs_GetOOClass(Tcl_Interp *interp, Tcl_Class *ooclass_cls)
{
  int err = TCL_OK;
  Tcl_Object ooclass_obj;
  Tcl_Obj *ooclass_str = Tcl_NewStringObj("::oo::class", -1);
  Tcl_IncrRefCount(ooclass_str);

  if ((ooclass_obj = Tcl_GetObjectFromObj(interp, ooclass_str)) == NULL) {
    err = TCL_ERROR;
    goto cleanup_str;
  }

  if ((*ooclass_cls = Tcl_GetObjectAsClass(ooclass_obj)) == NULL) {
    err = TCL_ERROR;
    goto cleanup_str;
  }

 cleanup_str:
  Tcl_DecrRefCount(ooclass_str);

  return err;
}

NTJOBS_EXPORT int Ntjobs_Init(Tcl_Interp *interp)
{
  if (Tcl_InitStubs(interp, "8.6", 0) == NULL) {
    return TCL_ERROR;
  }

  if (Tcl_OOInitStubs(interp) == NULL) {
    return TCL_ERROR;
  }

  Tcl_Class ooclass;
  TCLTRY(Ntjobs_GetOOClass(interp, &ooclass));

  Tcl_Object jobclass_obj;
  if ((jobclass_obj = Tcl_NewObjectInstance(
         interp, ooclass,
         "::nt::job", NULL,
         0, NULL, 0)) == NULL) {
    return TCL_ERROR;
  }

  Tcl_Class jobclass;
  if ((jobclass = Tcl_GetObjectAsClass(jobclass_obj)) == NULL) {
    goto error_cleanup_ntjob;
  }

  for (int k = 0; Ntjobs_JobMethods[k].version != -1; k++) {
    Tcl_Obj *name = NULL;
    if (Ntjobs_JobMethods[k].name != NULL) {
      name = Tcl_NewStringObj(Ntjobs_JobMethods[k].name, -1);
    }

    Tcl_Method method = Tcl_NewMethod(
      interp, jobclass,
      name, 1,
      &Ntjobs_JobMethods[k],
      NULL);

    if (k == 0) {
      // constructor
      Tcl_ClassSetConstructor(interp, jobclass, method);
    }
  }

  // TODO: get version from CMake
  Tcl_PkgProvide(interp, "ntjobs", "0.1");
  return TCL_OK;

 error_cleanup_ntjob:
  Tcl_UnsetVar(interp, "::nt::job", TCL_GLOBAL_ONLY);
  return TCL_ERROR;
}
