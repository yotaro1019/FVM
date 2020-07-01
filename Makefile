main: parameters.f90 main.f90 field.f90
	gfortran -c parameters.f90
	gfortran -c field.f90
	gfortran -o cfd  field.o parameters.o main.f90


clean:
	rm cfd
	rnm *.mod
	rm *.g
	rm *.q