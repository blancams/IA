HEADDIR = head
SRCDIR = src
BUILDDIR = build

MAIN = main
HEXT = hextree
MANC = mancala
HEUR = heuristic
WHDB = whdb
GENC = genetic
TEST = tests

CC = gcc
FLAGS = -g -Wall -std=c99 -I$(HEADDIR)

.PHONY: all clean

all: before $(MAIN)

before:
	@[ -d $(BUILDDIR) ] || mkdir $(BUILDDIR)

$(MAIN): $(BUILDDIR)/$(MAIN).o $(BUILDDIR)/$(HEXT).o $(BUILDDIR)/$(MANC).o $(BUILDDIR)/$(HEUR).o $(BUILDDIR)/$(WHDB).o $(BUILDDIR)/$(GENC).o $(BUILDDIR)/$(TEST).o
	@echo Linking $@: $^
	$(CC) -o $@ $^ $(FLAGS)

$(BUILDDIR)/%.o: $(SRCDIR)/%.c
	@echo Compilando $@: $^
	$(CC) -o $@ -c $^ $(FLAGS)

clean:
	@rm -f ./$(BUILDDIR)/* $(MAIN)
