// Hex helper library Tcl extension
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

#include <hex/hex.h>
#include <stdint.h>
#include <string.h>
#include <tcl.h>
#include <tclOO.h>


#define TCLTRY(expr) do {                       \
    if ((expr) != TCL_OK) {                     \
      return TCL_ERROR;                         \
    }                                           \
  } while (0)


void *tcl_calloc(size_t nmemb, size_t size) {
  if (size > SIZE_MAX / nmemb) {
    return NULL;
  }

  size_t count = nmemb * size;
  void *data = ckalloc(count);
  memset(data, 0, count);
  return data;
}


void hex_delete_board_metadata(ClientData metadata)
{
  hex_board *board = metadata;
  ckfree(metadata);
}


int hex_clone_board_metadata(
  Tcl_Interp *interp,
  ClientData srcMetadata,
  ClientData *dstMetadataPtr)
{
  hex_err err;
  hex_board *src = srcMetadata;
  hex_board *dest;
  int size = hex_board_size(src);

  if ((err = hex_board_init(&dest, size, tcl_calloc)) != HEX_OK) {
    Tcl_SetObjResult(interp, Tcl_ObjPrintf("!!BUG!! bad size %d", size));
    return TCL_ERROR;
  }

  if ((err = hex_board_copy(dest, src)) != HEX_OK) {
    Tcl_SetResult(interp, "!!BUG!! board size mismatch", TCL_STATIC);
    return TCL_ERROR;
  }

  *dstMetadataPtr = dest;
  return TCL_OK;
}


static Tcl_ObjectMetadataType hex_board_metadata = {
  .version = TCL_OO_METADATA_VERSION_CURRENT,
  .name = "hex_board_metadata",
  .deleteProc = &hex_delete_board_metadata,
  .cloneProc = &hex_clone_board_metadata,
};


int hex_context_to_board(
  Tcl_ObjectContext context,
  hex_board **board)
{
  Tcl_Object self;
  if ((self = Tcl_ObjectContextObject(context)) == NULL) {
    return TCL_ERROR;
  }

  *board = Tcl_ObjectGetMetadata(self, &hex_board_metadata);
  return TCL_OK;
}


int hex_board_ctor_method(
  ClientData clientData,
  Tcl_Interp *interp,
  Tcl_ObjectContext objectContext,
  int objc,
  Tcl_Obj *const *objv)
{
  Tcl_Object self;
  int size;
  hex_err err;
  hex_board *board;

  if ((self = Tcl_ObjectContextObject(objectContext)) == NULL) {
    return TCL_ERROR;
  }

  if (strcmp("new", Tcl_GetString(objv[1])) == 0) {
    if (objc != 3) {
      Tcl_WrongNumArgs(interp, 1, objv, "new size");
      return TCL_ERROR;
    }
  } else {
    if (objc != 4) {
      Tcl_WrongNumArgs(interp, 1, objv, "create name size");
      return TCL_ERROR;
    }
  }

  TCLTRY(Tcl_GetIntFromObj(interp, objv[objc-1], &size));

  if ((err = hex_board_init(&board, size, &tcl_calloc)) != HEX_OK) {
    Tcl_SetObjResult(interp, Tcl_ObjPrintf("bad size %d", size));
    return TCL_ERROR;
  }

  Tcl_ObjectSetMetadata(self, &hex_board_metadata, board);
  return TCL_OK;
}


int hex_board_size_method(
  ClientData clientData,
  Tcl_Interp *interp,
  Tcl_ObjectContext objectContext,
  int objc,
  Tcl_Obj *const *objv)
{
  hex_board *self;
  TCLTRY(hex_context_to_board(objectContext, &self));

  if (objc != 2) {
    Tcl_WrongNumArgs(interp, 1, objv, "size");
    return TCL_ERROR;
  }

  Tcl_SetObjResult(interp, Tcl_NewIntObj(hex_board_size(self)));
  return TCL_OK;
}


int hex_arg_coords(
  Tcl_Interp *interp,
  Tcl_Obj *lst,
  int *row, int *col)
{
  int objc;
  Tcl_Obj **objv;
  TCLTRY(Tcl_ListObjGetElements(interp, lst, &objc, &objv));
  if (objc != 2) {
    Tcl_SetResult(
      interp,
      "coordinate list must have exactly two elements",
      TCL_STATIC);
    return TCL_ERROR;
  }

  TCLTRY(Tcl_GetIntFromObj(interp, objv[0], row));
  TCLTRY(Tcl_GetIntFromObj(interp, objv[1], col));
  return TCL_OK;
}


Tcl_Obj *hex_color_to_obj(hex_color color) {
  switch (color) {
  case HEX_COLOR_RED:
    return Tcl_NewStringObj("red", -1);

  case HEX_COLOR_BLUE:
    return Tcl_NewStringObj("blue", -1);

  default:
    return Tcl_NewObj();
  }
}


int hex_board_get_method(
  ClientData clientData,
  Tcl_Interp *interp,
  Tcl_ObjectContext objectContext,
  int objc,
  Tcl_Obj *const *objv)
{
  hex_board *self;
  int len, row, col;
  hex_err err;
  Tcl_Obj *elem;
  hex_color *color;

  TCLTRY(hex_context_to_board(objectContext, &self));

  if (objc != 3) {
    Tcl_WrongNumArgs(interp, 1, objv, "get {row col}");
    return TCL_ERROR;
  }

  TCLTRY(hex_arg_coords(interp, objv[2], &row, &col));
  if ((err = hex_board_space(self, row, col, &color)) != HEX_OK) {
    Tcl_SetObjResult(interp, Tcl_ObjPrintf(
                       "coordinates {%d %d} out of bounds for size %d",
                       row, col, hex_board_size(self)));
    return TCL_ERROR;
  }

  Tcl_SetObjResult(interp, hex_color_to_obj(*color));
  return TCL_OK;
}


int hex_board_set_method(
  ClientData clientData,
  Tcl_Interp *interp,
  Tcl_ObjectContext objectContext,
  int objc,
  Tcl_Obj *const *objv)
{
  hex_board *self;
  int len, row, col;
  hex_err err;
  Tcl_Obj *elem;
  hex_color *color;
  char *new_color;

  TCLTRY(hex_context_to_board(objectContext, &self));

  if (objc != 4) {
    Tcl_WrongNumArgs(interp, 1, objv, "set {row col} color");
    return TCL_ERROR;
  }

  TCLTRY(hex_arg_coords(interp, objv[2], &row, &col));
  if ((err = hex_board_space(self, row, col, &color)) != HEX_OK) {
    Tcl_SetObjResult(interp, Tcl_ObjPrintf(
                       "coordinates {%d %d} out of bounds for size %d",
                       row, col, hex_board_size(self)));
    return TCL_ERROR;
  }

  new_color = Tcl_GetString(objv[3]);
  if (strcmp("red", new_color) == 0
      || strcmp("r", new_color) == 0) {
    *color = HEX_COLOR_RED;
  } else if (strcmp("blue", new_color) == 0
             || strcmp("b", new_color) == 0) {
    *color = HEX_COLOR_BLUE;
  } else if (strcmp("", new_color) == 0) {
    *color = HEX_COLOR_NONE;
  } else {
    Tcl_SetObjResult(interp, Tcl_ObjPrintf(
                       "bad color \"%s\"", new_color));
    return TCL_ERROR;
  }

  return TCL_OK;
}


Tcl_Obj *hex_coords_to_list(int row, int col)
{
  Tcl_Obj *coord_objs[2];
  coord_objs[0] = Tcl_NewIntObj(row);
  coord_objs[1] = Tcl_NewIntObj(col);
  return Tcl_NewListObj(2, coord_objs);
}


int hex_board_neighbors_method(
  ClientData clientData,
  Tcl_Interp *interp,
  Tcl_ObjectContext objectContext,
  int objc,
  Tcl_Obj *const *objv)
{
  hex_board *self;
  hex_err err;
  int row, col;
  int neighbors[12];
  int neighbor_count;
  Tcl_Obj *neighbor_objs[6];
  Tcl_Obj *coord_objs[2];

  TCLTRY(hex_context_to_board(objectContext, &self));

  if (objc != 3) {
    Tcl_WrongNumArgs(interp, 1, objv, "neighbors {row col}");
    return TCL_ERROR;
  }

  TCLTRY(hex_arg_coords(interp, objv[2], &row, &col));
  if ((err = hex_board_neighborcoords(
         self,
         row, col,
         neighbors, &neighbor_count))
      != HEX_OK) {
    Tcl_SetObjResult(interp, Tcl_ObjPrintf(
                       "coordinates {%d %d} out of bounds for size %d",
                       row, col, hex_board_size(self)));
    return TCL_ERROR;
  }

  for (int k = 0; k < neighbor_count; k++) {
    neighbor_objs[k] = hex_coords_to_list(
      neighbors[2*k + 0],
      neighbors[2*k + 1]);
  }

  Tcl_SetObjResult(interp, Tcl_NewListObj(neighbor_count, neighbor_objs));
  return TCL_OK;
}


int hex_board_clear_method(
  ClientData clientData,
  Tcl_Interp *interp,
  Tcl_ObjectContext objectContext,
  int objc,
  Tcl_Obj *const *objv)
{
  hex_board *self;
  TCLTRY(hex_context_to_board(objectContext, &self));

  if (objc != 2) {
    Tcl_WrongNumArgs(interp, 1, objv, "clear");
    return TCL_ERROR;
  }

  hex_board_clear(self);
  return TCL_OK;
}


int hex_board_winner_method(
  ClientData clientData,
  Tcl_Interp *interp,
  Tcl_ObjectContext objectContext,
  int objc,
  Tcl_Obj *const *objv)
{
  hex_board *self;
  hex_err err;
  hex_color winner;
  TCLTRY(hex_context_to_board(objectContext, &self));

  if (objc != 2) {
    Tcl_WrongNumArgs(interp, 1, objv, "winner");
    return TCL_ERROR;
  }

  if ((err = hex_board_getwinner(self, &winner)) != HEX_OK) {
    Tcl_SetResult(interp, "error while finding winner", TCL_STATIC);
    return TCL_ERROR;
  }

  Tcl_SetObjResult(interp, hex_color_to_obj(winner));
  return TCL_OK;
}


int hex_board_coords_method(
  ClientData clientData,
  Tcl_Interp *interp,
  Tcl_ObjectContext objectContext,
  int objc,
  Tcl_Obj *const *objv)
{
  hex_board *self;
  int size, lstc;
  Tcl_Obj **lstv;
  Tcl_Obj **lstp;
  TCLTRY(hex_context_to_board(objectContext, &self));

  if (objc != 2) {
    Tcl_WrongNumArgs(interp, 1, objv, "coords");
    return TCL_ERROR;
  }

  size = hex_board_size(self);
  lstc = size * size * 2;
  lstv = tcl_calloc(lstc, sizeof(Tcl_Obj *));

  lstp = lstv;
  for (int c = 0; c < size; c++) {
    for (int r = 0; r < size; r++) {
      *(lstp++) = Tcl_NewIntObj(r);
      *(lstp++) = Tcl_NewIntObj(c);
    }
  }

  Tcl_SetObjResult(interp, Tcl_NewListObj(lstc, lstv));
  ckfree(lstv);
  return TCL_OK;
}


int hex_board_dump_method(
  ClientData clientData,
  Tcl_Interp *interp,
  Tcl_ObjectContext objectContext,
  int objc,
  Tcl_Obj *const *objv)
{
  hex_board *self;
  int err = TCL_OK;
  Tcl_Obj *dump_obj;
  Tcl_DString dump;
  int size;
  TCLTRY(hex_context_to_board(objectContext, &self));

  if (objc != 2) {
    Tcl_WrongNumArgs(interp, 1, objv, "dump");
    return TCL_ERROR;
  }

  size = hex_board_size(self);

  Tcl_DStringInit(&dump);
  Tcl_DStringSetLength(&dump, size * size);

  if (hex_board_dump(
        self, Tcl_DStringValue(&dump), Tcl_DStringLength(&dump) + 1)
      != HEX_OK)
  {
    Tcl_SetResult(interp, "bug in internal size calculation", TCL_STATIC);
    err = TCL_ERROR;
    goto cleanup;
  }

  Tcl_DStringResult(interp, &dump);

 cleanup:
  Tcl_DStringFree(&dump);

  return err;
}


int hex_board_scan_method(
  ClientData clientData,
  Tcl_Interp *interp,
  Tcl_ObjectContext objectContext,
  int objc,
  Tcl_Obj *const *objv)
{
  hex_board *self;
  TCLTRY(hex_context_to_board(objectContext, &self));

  if (objc != 3) {
    Tcl_WrongNumArgs(interp, 1, objv, "scan board");
    return TCL_ERROR;
  }

  if (hex_board_scan(self, Tcl_GetString(objv[2])) != HEX_OK) {
    Tcl_SetResult(interp, "board size mismatch", TCL_STATIC);
    return TCL_ERROR;
  }

  return TCL_OK;
}


static Tcl_MethodType board_methods[] = {
  { TCL_OO_METHOD_VERSION_CURRENT, NULL, hex_board_ctor_method },
  { TCL_OO_METHOD_VERSION_CURRENT, "size", hex_board_size_method },
  { TCL_OO_METHOD_VERSION_CURRENT, "get", hex_board_get_method },
  { TCL_OO_METHOD_VERSION_CURRENT, "set", hex_board_set_method },
  { TCL_OO_METHOD_VERSION_CURRENT, "neighbors", hex_board_neighbors_method },
  { TCL_OO_METHOD_VERSION_CURRENT, "clear", hex_board_clear_method },
  { TCL_OO_METHOD_VERSION_CURRENT, "winner", hex_board_winner_method },
  { TCL_OO_METHOD_VERSION_CURRENT, "coords", hex_board_coords_method },
  { TCL_OO_METHOD_VERSION_CURRENT, "dump", hex_board_dump_method },
  { TCL_OO_METHOD_VERSION_CURRENT, "scan", hex_board_scan_method },
  { -1 },
};


static int get_oo_class(Tcl_Interp *interp, Tcl_Class *ooclass_cls)
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


int Hex_Init(Tcl_Interp *interp)
{
  int err;

  // May be possible to use an older Tcl version, but 8.6 is what I have handy
  // on my machine.
  if (Tcl_InitStubs(interp, "8.6", 0) == NULL) {
    return TCL_ERROR;
  }

  if (Tcl_OOInitStubs(interp) == NULL) {
    return TCL_ERROR;
  }

  Tcl_Class ooclass;
  if ((err = get_oo_class(interp, &ooclass)) != TCL_OK) {
    return err;
  }

  Tcl_Object boardclass_obj;
  if ((boardclass_obj = Tcl_NewObjectInstance(
         interp, ooclass,
         "::hex::board", NULL,
         0, NULL, 0)) == NULL) {
    return TCL_ERROR;
  }

  Tcl_Class boardclass;
  if ((boardclass = Tcl_GetObjectAsClass(boardclass_obj)) == NULL) {
    goto error_cleanup_hexboard;
  }

  for (int k = 0; board_methods[k].version != -1; k++) {
    Tcl_Obj *name = NULL;
    if (board_methods[k].name != NULL) {
      name = Tcl_NewStringObj(board_methods[k].name, -1);
    }

    Tcl_Method method = Tcl_NewMethod(
      interp, boardclass,
      name, 1,
      &board_methods[k],
      NULL);

    if (k == 0) {
      // constructor
      Tcl_ClassSetConstructor(interp, boardclass, method);
    }
  }

  // TODO: get version from CMake
  Tcl_PkgProvide(interp, "hex", "0.1");
  return TCL_OK;

 error_cleanup_hexboard:
  Tcl_UnsetVar(interp, "::hex::board", TCL_GLOBAL_ONLY);
  return TCL_ERROR;
}

