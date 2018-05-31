include ../Makefile.shared

COMPILER=ocamlc.opt

SOURCE_LIST= caml_hash \
	js_math \
	js \
	belt_Option \
	belt_Range \
	belt_Id \
	belt_Array \
	belt_SortArrayInt \
	belt_SortArrayString \
	belt_internalBucketsType \
	belt_internalSetBuckets \
	belt_internalBuckets \
	belt_HashSetString \
	belt_HashSetInt \
	belt_MutableStack \
	belt_MutableQueue \
	belt_HashMapInt \
	belt_HashMapString \
	belt_HashSet \
	belt_HashMap \
	belt_SortArray \
	belt_List \
	belt_internalAVLset \
	belt_internalAVLtree \
	belt_internalSetString \
	belt_internalSetInt \
	belt_MutableSetString \
	belt_MutableSetInt \
	belt_MutableSet \
	belt_internalMapString \
	belt_internalMapInt \
	belt_SetDict \
	belt_MapDict \
	belt_MapString \
	belt_MapInt \
	belt_MutableMapInt \
	belt_MutableMapString \
	belt_SetInt \
	belt_SetString \
	belt_Set \
	belt_MutableMap \
	belt_Map \
	belt

# belt_internalSet\
# belt_internalMutableAVL\
# belt_Bag\
# belt_BagM\
# belt_dyn belt_dyn_lib\

OBJECT_FILES := $(addsuffix .cmo, $(SOURCE_LIST))
RUNTIME := $(OBJECT_FILES) $(addsuffix .cmi, $(SOURCE_LIST))

INCLUDES=

BS_FLAGS=  $(BS_COMMON_FLAGS)
COMPFLAGS += $(BS_FLAGS)  -g -w +3-40-49 -warn-error A -bin-annot -ppx ../../lib/belt_bsppx.exe

all: $(RUNTIME)
	$(COMPILER) -a -o lib.cma $(OBJECT_FILES)
	
stubs.o: stubs.c
	$(COMPILER) -c $^

$(RUNTIME): ../../lib/belt_bsppx.exe stubs.o

OCAML_SRC_UTILS=../../vendor/ocaml/utils
OCAML_SRC_PARSING=../../vendor/ocaml/parsing
OCAML_SRC_TYPING=../../vendor/ocaml/typing
OCAML_SRC_BYTECOMP=../../vendor/ocaml/bytecomp
OCAML_SRC_DRIVER=../../vendor/ocaml/driver
OCAML_SRC_TOOLS=../../vendor/ocaml/tools

../../lib/belt_bsppx.exe:
	make -C belt_bsppx force-snapshotml
	make -C ../../lib belt_bsppx.exe

clean::
	rm -f *.cm* *.o
	rm -f *~
	rm -f  *.annot
	rm -f *.rawlambda *.lam *.lambda *.map
	rm -rf belt_HashSetString.ml belt_HashSetString.mli belt_HashSetInt.ml  belt_HashSetInt.mli \
	belt_HashMapInt.ml belt_HashMapInt.mli  belt_HashMapString.ml belt_HashMapString.mli \
	belt_MapInt.ml belt_MapInt.mli  belt_MapString.ml belt_MapString.mli \
	belt_internalSetInt.ml belt_internalSetString.ml \
	belt_SetInt.ml belt_SetInt.mli belt_SetString.ml  belt_SetString.mli \
	belt_MutableSetInt.ml belt_MutableSetInt.mli belt_MutableSetString.ml belt_MutableSetString.mli\
	belt_SortArrayInt.ml belt_SortArrayInt.mli belt_SortArrayString.ml belt_SortArrayString.mli\
	belt_internalMapInt.ml belt_internalMapInt.mli \
	belt_internalMapIntM.ml belt_internalMapIntM.mli \
	belt_internalMapString.ml belt_internalMapString.mli\
	belt_internalMapStringM.ml belt_internalMapStringM.mli\
	belt_MutableMapString.mli belt_MutableMapString.ml\
	belt_MutableMapInt.mli belt_MutableMapInt.ml

ifndef BS_RELEASE_BUILD
belt_HashSetString.ml: hashset.cppo.ml
	cppo -D TYPE_STRING $^ > $@
belt_HashSetInt.ml: hashset.cppo.ml
	cppo -D TYPE_INT $^ > $@
belt_HashSetString.mli: hashset.cppo.mli
	cppo -D TYPE_STRING $^ > $@
belt_HashSetInt.mli: hashset.cppo.mli
	cppo -D TYPE_INT $^ > $@
belt_HashMapString.ml: hashmap.cppo.ml
	cppo -D TYPE_STRING $^ > $@
belt_HashMapInt.ml: hashmap.cppo.ml
	cppo -D TYPE_INT $^ > $@
belt_HashMapString.mli: hashmap.cppo.mli
	cppo -D TYPE_STRING $^ > $@
belt_HashMapInt.mli: hashmap.cppo.mli
	cppo -D TYPE_INT $^ > $@
belt_MapString.ml: map.cppo.ml
	cppo -D TYPE_STRING $^ > $@
belt_MapInt.ml: map.cppo.ml
	cppo -D TYPE_INT $^ > $@
belt_MapString.mli: map.cppo.mli
	cppo -D TYPE_STRING $^ > $@
belt_MapInt.mli: map.cppo.mli
	cppo -D TYPE_INT $^ > $@
belt_MutableMapString.mli: mapm.cppo.mli
	cppo -D TYPE_STRING $^ > $@
belt_MutableMapInt.mli: mapm.cppo.mli
	cppo -D TYPE_INT $^ > $@
belt_MutableMapString.ml: mapm.cppo.ml
	cppo -D TYPE_STRING $^ > $@
belt_MutableMapInt.ml: mapm.cppo.ml
	cppo -D TYPE_INT $^ > $@
belt_internalMapInt.ml : internal_map.cppo.ml
	cppo -D TYPE_INT $^ > $@
belt_internalMapString.ml : internal_map.cppo.ml
	cppo -D TYPE_STRING $^ > $@
belt_internalSetInt.ml: internal_set.cppo.ml
	cppo -D TYPE_INT $^ > $@
belt_internalSetString.ml: internal_set.cppo.ml
	cppo -D TYPE_STRING $^ > $@

belt_SetInt.ml: set.cppo.ml
	cppo -D TYPE_INT $^ > $@
belt_SetString.ml: set.cppo.ml
	cppo -D TYPE_STRING $^ > $@

belt_SetInt.mli: set.cppo.mli
	cppo -D TYPE_INT $^ > $@
belt_SetString.mli: set.cppo.mli
	cppo -D TYPE_STRING $^ > $@

belt_MutableSetInt.ml: setm.cppo.ml
	cppo -D TYPE_INT $^ > $@
belt_MutableSetString.ml: setm.cppo.ml
	cppo -D TYPE_STRING $^ > $@
belt_MutableSetInt.mli: setm.cppo.mli
	cppo -D TYPE_INT $^ > $@
belt_MutableSetString.mli: setm.cppo.mli
	cppo -D TYPE_STRING $^ > $@

belt_SortArrayInt.ml : sort.cppo.ml
	cppo -D TYPE_INT $^ >$@
belt_SortArrayString.ml : sort.cppo.ml
	cppo -D TYPE_STRING $^ >$@
belt_SortArrayInt.mli : sort.cppo.mli
	cppo -D TYPE_INT $^ > $@
belt_gSortArrayString.mli : sort.cppo.mli
	cppo -D TYPE_STRING $^ > $@
endif	


.mli.cmi:
	$(COMPILER) $(INCLUDES) $(COMPFLAGS)  -c $<
.ml.cmo:
	$(COMPILER) $(INCLUDES) $(COMPFLAGS)  -c $<


-include .depend

ML_SOURCES=$(addsuffix .ml, $(SOURCE_LIST))
MLI_SOURCES=$(addsuffix .mli, $(SOURCE_LIST))

depend:
	$(CAMLDEP) -native $(INCLUDES) $(ML_SOURCES) $(MLI_SOURCES) | sed -e 's/\.cmx/.cmo/g' >.depend
