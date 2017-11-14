# add the CUDA installation into our path.
CUDA_ROOT=/usr/local/cuda

if [ -d "${CUDA_ROOT}" ]; then
    export PATH=${PATH}:"${CUDA_ROOT}/bin"
    export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:"${CUDA_ROOT}/lib64"
    export MANPATH=${MANPATH}:"${CUDA_ROOT}"/doc/man
fi

# add the CUDA SDK examples into our path.
CUDA_SAMPLES_ROOT=${HOME}/cuda-9.0/bin

if [ -d "${CUDA_SAMPLES_ROOT}" ]; then
    export PATH=${PATH}:"${CUDA_SAMPLES_ROOT}"/bin
fi
