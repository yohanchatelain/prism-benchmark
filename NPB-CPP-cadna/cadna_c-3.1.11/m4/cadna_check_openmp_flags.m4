AC_DEFUN([CADNA_CHECK_OPENMP_FLAGS],[


AC_MSG_CHECKING([OpenMP specific flags $OPENMP_CXXFLAGS ])

CADNA_CHECK_OPENMP_CXXFLAGS="$CXXFLAGS"

CXXFLAGS="$CXXFLAGS $AM_CXXFLAGS $OPENMP_CXXFLAGS  -Werror "



AC_RUN_IFELSE([
        AC_LANG_PROGRAM(
                [[ #include "omp.h" ]], 
                [
			[int i,  arr[100];

       			 #pragma omp parallel for 
                	 for (i=0; i<100; i++) arr[i]=i;
                    	 return 0;
		  ]
		  ]
	   )
	   ],
                    [
                    use_openmp="yes"
                    CXXCADNACOMMONFLAGS="$CXXCADNACOMMONFLAGS  "
                    AC_MSG_RESULT(yes)  
                    ],
                    [
                     use_openmp="yes" 
                     AC_MSG_RESULT(yes)
                    ])

])

CXXFLAGS="$CADNA_CHECK_OPENMP_CXXFLAGS"

