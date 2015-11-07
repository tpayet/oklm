#*****************************************************************************#

NAME = ft_cipher

OCAMLC = ocamlc

LGRAPHICS = graphics.cma

SRCS = cipher.ml uncipher.ml

SRCO = $(SRCS:.ml=.cmo)

SRCI = $(SRCS:.ml=.cmi)

.PHONY: clean fclean re

all: $(NAME)

$(NAME):
		$(OCAMLC) $(SRCS) -o $(NAME)

clean:
		rm -f $(SRCO) $(SRCI)

fclean: clean
		rm -f $(NAME)

re: fclean all