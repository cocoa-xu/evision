#ifndef EVISION_CONSTS_H
#pragma once

#include <erl_nif.h>

// nil
inline ERL_NIF_TERM kAtomNil;
// true
inline ERL_NIF_TERM kAtomTrue;
// false
inline ERL_NIF_TERM kAtomFalse;
// out_of_memory
inline ERL_NIF_TERM kAtomOutOfMemory;
// ref
inline ERL_NIF_TERM kAtomRef;
// ref_data
inline ERL_NIF_TERM kAtomRefData;
// class
inline ERL_NIF_TERM kAtomClass;
// dims
inline ERL_NIF_TERM kAtomDims;
// channels
inline ERL_NIF_TERM kAtomChannels;
// type
inline ERL_NIF_TERM kAtomType;
// data
inline ERL_NIF_TERM kAtomData;
// raw_type
inline ERL_NIF_TERM kAtomRawType;
// elemSize
inline ERL_NIF_TERM kAtomElemSize;
// step
inline ERL_NIF_TERM kAtomStep;
// device_id
inline ERL_NIF_TERM kAtomDeviceID;
// shape
inline ERL_NIF_TERM kAtomShape;
// __struct__
inline ERL_NIF_TERM kAtomStructKey;
// Elixir.Nx.Tensor
inline ERL_NIF_TERM kAtomNxTensorModule;
// nx_tensor (only for internal use)
inline ERL_NIF_TERM kAtomNxTensor;
// Elixir.Nx.Tensor
inline ERL_NIF_TERM kAtomEvisionMatModule;

// atoms for types
inline ERL_NIF_TERM kAtomU;
inline ERL_NIF_TERM kAtomS;
inline ERL_NIF_TERM kAtomF;
inline ERL_NIF_TERM kAtomU8;
inline ERL_NIF_TERM kAtomS8;
inline ERL_NIF_TERM kAtomU16;
inline ERL_NIF_TERM kAtomS16;
inline ERL_NIF_TERM kAtomU32;
inline ERL_NIF_TERM kAtomS32;
inline ERL_NIF_TERM kAtomU64;
inline ERL_NIF_TERM kAtomS64;
inline ERL_NIF_TERM kAtomF16;
inline ERL_NIF_TERM kAtomF32;
inline ERL_NIF_TERM kAtomF64;
inline ERL_NIF_TERM kAtomBF;
inline ERL_NIF_TERM kAtomBF16;
inline ERL_NIF_TERM kAtomF8E4M3FN;
inline ERL_NIF_TERM kAtomUser;

// atoms for cv::Moments
inline ERL_NIF_TERM kAtomM00;
inline ERL_NIF_TERM kAtomM10;
inline ERL_NIF_TERM kAtomM01;
inline ERL_NIF_TERM kAtomM20;
inline ERL_NIF_TERM kAtomM11;
inline ERL_NIF_TERM kAtomM02;
inline ERL_NIF_TERM kAtomM30;
inline ERL_NIF_TERM kAtomM21;
inline ERL_NIF_TERM kAtomM12;
inline ERL_NIF_TERM kAtomM03;
inline ERL_NIF_TERM kAtomMu20;
inline ERL_NIF_TERM kAtomMu11;
inline ERL_NIF_TERM kAtomMu02;
inline ERL_NIF_TERM kAtomMu30;
inline ERL_NIF_TERM kAtomMu21;
inline ERL_NIF_TERM kAtomMu12;
inline ERL_NIF_TERM kAtomMu03;
inline ERL_NIF_TERM kAtomNu20;
inline ERL_NIF_TERM kAtomNu11;
inline ERL_NIF_TERM kAtomNu02;
inline ERL_NIF_TERM kAtomNu30;
inline ERL_NIF_TERM kAtomNu21;
inline ERL_NIF_TERM kAtomNu12;
inline ERL_NIF_TERM kAtomNu03;

#endif  // EVISION_CONSTS_H
