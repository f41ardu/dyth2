all: dyth

OBJ = dyth.o gtheta.o c.o myeb.o bragg.o 

CFLAGS = -r8R

dyth: $(OBJ)
	gfortran $(CFLAGS) -o dyth $(OBJ) $(LDFLAGS)

clean: 
	rm -f dyth $(OBJ)

    # This is an explicit suffix rule. It may be omitted on systems
    # that handle simple rules like this automatically.
.f.o:
	gfortran $(CFLAGS) -c $<

FRC: 
	.SUFFIXES: .f

