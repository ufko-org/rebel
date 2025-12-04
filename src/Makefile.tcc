# TCC Makefile for Rebel BSD

OBJS = rebel.o \
			 rebel-symbol.o \
			 rebel-math.o \
			 rebel-list.o \
			 rebel-liststr.o \
			 rebel-string.o \
			 rebel-filesys.o \
			 rebel-sock.o \
			 rebel-import.o \
			 rebel-xml-json.o \
			 rebel-web.o \
			 rebel-matrix.o \
			 rebel-debug.o \
			 rebel-utf8.o \
			 pcre.o

CC = tcc

CFLAGS = -std=c90 -Wall -Oz -c \
	-I/usr/local/include \
	-D_BSD -DFFI -DREADLINE -DSUPPORT_UTF8 

default: rebel

rebel: $(OBJS)
	$(CC) $(OBJS) -lm -lreadline -lffi -L/usr/local/lib -o $@
	strip rebel
	ls -l rebel

.c.o:
	$(CC) $(CFLAGS) $<

$(OBJS): primes.h protos.h Makefile

clean:
	rm -f rebel *.orig $(OBJS)

test: rebel
	./rebel qa-dot
