all: dyth

OBJ = dyth.o gtheta.o c.o myeb.o bragg.o 

CFLAGS = -r8R -O4 -funroll-loops

L = -lf2c -lm -lc

dyth: $(OBJ)
	g77 -o dyth $(OBJ) $(LDFLAGS)

clean: 
	rm -f dyth $(OBJ) $(L)

    # This is an explicit suffix rule. It may be omitted on systems
    # that handle simple rules like this automatically.
.f.o:
	g77 $(CFLAGS) -c $<

FRC: 
	.SUFFIXES: .f

