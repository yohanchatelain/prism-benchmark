INCLUDE=-I${VIRTUAL_ENV}/include
LIB=-L${VIRTUAL_ENV}/lib

FLAGS_OPT=-O3 -march=native
FLAGS_DEBUG=-Og

PRSIM_SR_DEBUG_FLAGS="--online-instrumentation=sr --online-instrumentation-dispatch=dynamic --verbose"
PRSIM_SR_PERF_FLAGS="--online-instrumentation=sr --online-instrumentation-dispatch=static --verbose"

PRISM_UD_DEBUG_FLAGS="--online-instrumentation=up-down --online-instrumentation-dispatch=dynamic --verbose"
PRISM_UD_PERF_FLAGS="--online-instrumentation=up-down --online-instrumentation-dispatch=dynamic --verbose"

SRC=src/harmonic.c
CADNA_SRC=src/harmonic_cadna.c
SR_SRC=src/harmonic_sr.cpp

all: baseline double cadna sr verificarlo prism-sr prism-ud

baseline:
	$(CXX) $(SRC) ${FLAGS_DEBUG} -o harmonic-dbg -I${VIRTUAL_ENV}/include -L${VIRTUAL_ENV}/lib
	$(CXX) $(SRC) ${FLAGS_OPT} -o harmonic-perf -I${VIRTUAL_ENV}/include -L${VIRTUAL_ENV}/lib

double:
	$(CXX) $(SRC) -DDOUBLE ${FLAGS_DEBUG} -o harmonic_double-dbg -I${VIRTUAL_ENV}/include -L${VIRTUAL_ENV}/lib
	$(CXX) $(SRC) -DDOUBLE ${FLAGS_OPT} -o harmonic_double-perf -I${VIRTUAL_ENV}/include -L${VIRTUAL_ENV}/lib

cadna:
	$(CXX) $(CADNA_SRC) -lcadnaC ${FLAGS_DEBUG} -o harmonic_cadna-dbg -I${VIRTUAL_ENV}/include -L${VIRTUAL_ENV}/lib
	$(CXX) $(CADNA_SRC) -lcadnaC ${FLAGS_OPT} -o harmonic_cadna-perf -I${VIRTUAL_ENV}/include -L${VIRTUAL_ENV}/lib

sr:
	$(CXX) $(SR_SRC) -std=c++20 -lsr -lm ${FLAGS_DEBUG} -o harmonic_sr-dbg -I${VIRTUAL_ENV}/include -L${VIRTUAL_ENV}/lib
	$(CXX) $(SR_SRC) -std=c++20 -lsr -lm ${FLAGS_OPT} -o harmonic_sr-perf -I${VIRTUAL_ENV}/include -L${VIRTUAL_ENV}/lib

verificarlo:
	verificarlo-c $(SRC) ${FLAGS_DEBUG} -o harmonic_verificarlo-dbg -I${VIRTUAL_ENV}/include -L${VIRTUAL_ENV}/lib
	verificarlo-c $(SRC) ${FLAGS_OPT} -o harmonic_verificarlo-perf -I${VIRTUAL_ENV}/include -L${VIRTUAL_ENV}/lib

prism-sr:
	verificarlo-c $(SRC) ${FLAGS_DEBUG} ${PRSIM_SR_DEBUG_FLAGS} -o harmonic_prism_sr-dbg -I${VIRTUAL_ENV}/include -L${VIRTUAL_ENV}/lib
	verificarlo-c $(SRC) ${FLAGS_OPT} ${PRSIM_SR_PERF_FLAGS} -o harmonic_prism_sr-perf -I${VIRTUAL_ENV}/include -L${VIRTUAL_ENV}/lib

prism-ud:
	verificarlo-c $(SRC) ${FLAGS_DEBUG} ${PRISM_UD_DEBUG_FLAGS} -o harmonic_prism_ud-dbg -I${VIRTUAL_ENV}/include -L${VIRTUAL_ENV}/lib
	verificarlo-c $(SRC) ${FLAGS_OPT} ${PRISM_UD_PERF_FLAGS} -o harmonic_prism_ud-perf -I${VIRTUALENV}/include -L${VIRTUAL_ENV}/lib

clean:
	rm -f *.o *-dbg *-perf
