-include user.cfg

HAVE_APRON?=true
HAVE_Z3?=false

LIBS=-libs graph,str,nums

LIBPATH_APRON=
PP_OPTS_APRON=
ifeq (${HAVE_APRON},true)
  LIBPATH_APRON=-package apron -cflags -I,+apron -cflags -I,+mlgmpidl -lflags -I,+apron -lflags -I,+mlgmpidl
  LIBS=-libs graph,str,nums,gmp,apron,boxMPQ,octD
  PP_OPTS_APRON=-DHAVE_APRON
endif

LIBPATH_Z3=
PP_OPTS_Z3=
ifeq (${HAVE_Z3},true)
  LIBPATH_Z3= -package Z3 -cflags -I,+Z3 -lflags -I,+Z3 -lflags "-cclib -lz3"
  LIBS=-libs graph,str
  PP_OPTS_Z3=-DHAVE_Z3
endif

ifeq (${HAVE_Z3},true)
  ifeq (${HAVE_APRON},true)
	LIBS=-libs graph,str,gmp,apron,boxMPQ,octD
  endif
endif

LIBPATH=-package ocamlgraph -package yojson -package unix $(LIBPATH_APRON) $(LIBPATH_Z3)
PP_OPTS=-pp "camlp4o pa_macro.cmo $(PP_OPTS_APRON) $(PP_OPTS_Z3)"

OPTS=${PP_OPTS} -use-ocamlfind -cflags -warn-error,+a

default: kittel koat

all: kittel koat convert koatCConv koatFSTConv koatCESConv dep

arity: arity.ml
	ocamlbuild ${OPTS} ${LIBPATH} ${LIBS} arity.native

fixArity: fixArity.ml
	ocamlbuild ${OPTS} ${LIBPATH} ${LIBS} fixArity.native

kittel: make_git_sha1 force_look
	ocamlbuild ${OPTS} ${LIBPATH} ${LIBS} kittel.native

kittel.d.byte: make_git_sha1 force_look
	ocamlbuild ${OPTS} ${LIBPATH} ${LIBS} kittel.d.byte

koat: make_git_sha1 force_look
	ocamlbuild ${OPTS} ${LIBPATH} ${LIBS} koat.native

dep: dep.ml
	ocamlbuild ${OPTS} ${LIBPATH} ${LIBS} dep.native

drawRules: drawRules.ml
	ocamlbuild ${OPTS} ${LIBPATH} ${LIBS} drawRules.native

koat.d.byte: make_git_sha1 force_look
	ocamlbuild ${OPTS} ${LIBPATH} ${LIBS} koat.d.byte

convert: force_look
	ocamlbuild ${OPTS} ${LIBPATH} ${LIBS} convert.native

koatCConv: force_look
	ocamlbuild ${OPTS} ${LIBPATH} ${LIBS} koatCConv.native

koatFSTConv: force_look
	ocamlbuild ${OPTS} ${LIBPATH} ${LIBS} koatFSTConv.native

koatCESConv: force_look
	ocamlbuild ${OPTS} ${LIBPATH} ${LIBS} koatCESConv.native


test: force_look
	cd tests; sh runExamples.sh; sh simpleTest.sh

clean: force_look
	ocamlbuild -clean
	-rm -r test/out
	rm -f git_sha1.ml
	-rm -f *\~
	-rm -f */*\~

make_git_sha1: force_look
	./make_git_sha1.sh git_sha1.ml

force_look:
	@true
