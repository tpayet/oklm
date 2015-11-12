#*****************************************************************************#

NAME = ex02

SRC = people.ml doctor.ml dalek.ml main.ml

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)
TMP = $(OBJS) $(OPTOBJS) $(SRC:.ml=.cmi) $(SRC:.ml=.o)
DEPS = .depend

LIBS = 
WITHGRAPHICS = graphics.cma -cclib -lgraphics
WITHUNIX = unix.cma -cclib -lunix
WITHSTR = str.cma -cclib -lstr
WITHNUMS = nums.cma -cclib -lnums
WITHTHREADS = threads.cma -cclib -lthreads
WITHDBM = dbm.cma -cclib -lmldbm -cclib -lndbm

RM = rm -rf
MV = mv -f
MKDIR = mkdir -p

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep
CFLAGS = -w +a-4-6-7-9-27-29-32..39-41..42-44-45 -warn-error -a

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx
.PHONY: all me_cry opt clean fclean depend re

all: depend $(NAME)

me_cry: CFLAGS = -w @A
me_cry: depend $(NAME)

-include $(DEPS)

$(NAME): $(OBJS)
	$(CAMLC) $(CFLAGS) $(LIBS) $(OBJS) -o $(NAME)

opt: $(OPTOBJS)
	$(CAMLOPT) $(CFLAGS) $(LIBS:.cma=.cmxa) $(OPTOBJS) -o $(NAME)

%.cmo: %.ml
	$(CAMLC) $(CFLAGS) -c $< -o $@

%.cmx: %.ml
	$(CAMLOPT) $(CFLAGS) -c $< -o $@

clean:
	$(RM) $(TMP) $(DEPS)

fclean: clean
	$(RM) $(NAME)

depend: $(DEPS)
$(DEPS):
	$(CAMLDEP) $(SRCC) > $(DEPS)

re: fclean
	$(MAKE) all