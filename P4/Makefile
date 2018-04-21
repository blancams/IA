HEADDIR = head
SRCDIR = src
BUILDDIR = build

MAIN = main
HEXT = hextree
MANC = mancala
HEUR = heuristic

CC = gcc
FLAGS = -Wall -std=c99 -O2 -I$(HEADDIR)

.PHONY: all

all: $(MAIN)

$(MAIN): $(BUILDDIR)/$(MAIN).o $(BUILDDIR)/$(HEXT).o $(BUILDDIR)/$(MANC).o $(BUILDDIR)/$(HEUR).o
	@echo Linking $@: $^
	$(CC) -o $@ $^ $(FLAGS)

$(BUILDDIR)/%.o: $(SRCDIR)/%.c
	@echo Compilando $@: $^
	$(CC) -o $@ -c $^ $(FLAGS)