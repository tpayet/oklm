#*****************************************************************************#

NAME = gardening

OCAMLC = ocamlc

LGRAPHICS = graphics.cma

SRCS = gardening.ml

SRCO = $(SRCS:.ml=.cmo)

SRCI = $(SRCS:.ml=.cmi)

.PHONY: clean fclean re

all: $(NAME)

$(NAME):
		$(OCAMLC) $(LGRAPHICS) $(SRCS) -o $(NAME)

clean:
		rm -f $(SRCO) $(SRCI)

fclean: clean
		rm -f $(NAME)

re: fclean all