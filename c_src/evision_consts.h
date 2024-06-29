#ifndef EVISION_CONSTS_H
#pragma once

#include <erl_nif.h>

// nil
static ERL_NIF_TERM kAtomNil;
// true
static ERL_NIF_TERM kAtomTrue;
// false
static ERL_NIF_TERM kAtomFalse;
// out_of_memory
static ERL_NIF_TERM kAtomOutOfMemory;
// ref
static ERL_NIF_TERM kAtomRef;
// class
static ERL_NIF_TERM kAtomClass;
// dims
static ERL_NIF_TERM kAtomDims;
// channels
static ERL_NIF_TERM kAtomChannels;
// type
static ERL_NIF_TERM kAtomType;
// data
static ERL_NIF_TERM kAtomData;
// raw_type
static ERL_NIF_TERM kAtomRawType;
// elemSize
static ERL_NIF_TERM kAtomElemSize;
// step
static ERL_NIF_TERM kAtomStep;
// device_id
static ERL_NIF_TERM kAtomDeviceID;
// shape
static ERL_NIF_TERM kAtomShape;
// __struct__
static ERL_NIF_TERM kAtomStructKey;
// Elixir.Nx.Tensor
static ERL_NIF_TERM kAtomNxTensorModule;
// nx_tensor (only for internal use)
static ERL_NIF_TERM kAtomNxTensor;
// Elixir.Nx.Tensor
static ERL_NIF_TERM kAtomEvisionMatModule;

// atoms for types
static ERL_NIF_TERM kAtomU;
static ERL_NIF_TERM kAtomS;
static ERL_NIF_TERM kAtomF;
static ERL_NIF_TERM kAtomU8;
static ERL_NIF_TERM kAtomS8;
static ERL_NIF_TERM kAtomU16;
static ERL_NIF_TERM kAtomS16;
static ERL_NIF_TERM kAtomU32;
static ERL_NIF_TERM kAtomS32;
static ERL_NIF_TERM kAtomU64;
static ERL_NIF_TERM kAtomS64;
static ERL_NIF_TERM kAtomF16;
static ERL_NIF_TERM kAtomF32;
static ERL_NIF_TERM kAtomF64;
static ERL_NIF_TERM kAtomUser;

// atoms for cv::Moments
static ERL_NIF_TERM kAtomM00;
static ERL_NIF_TERM kAtomM10;
static ERL_NIF_TERM kAtomM01;
static ERL_NIF_TERM kAtomM20;
static ERL_NIF_TERM kAtomM11;
static ERL_NIF_TERM kAtomM02;
static ERL_NIF_TERM kAtomM30;
static ERL_NIF_TERM kAtomM21;
static ERL_NIF_TERM kAtomM12;
static ERL_NIF_TERM kAtomM03;
static ERL_NIF_TERM kAtomMu20;
static ERL_NIF_TERM kAtomMu11;
static ERL_NIF_TERM kAtomMu02;
static ERL_NIF_TERM kAtomMu30;
static ERL_NIF_TERM kAtomMu21;
static ERL_NIF_TERM kAtomMu12;
static ERL_NIF_TERM kAtomMu03;
static ERL_NIF_TERM kAtomNu20;
static ERL_NIF_TERM kAtomNu11;
static ERL_NIF_TERM kAtomNu02;
static ERL_NIF_TERM kAtomNu30;
static ERL_NIF_TERM kAtomNu21;
static ERL_NIF_TERM kAtomNu12;
static ERL_NIF_TERM kAtomNu03;

#endif  // EVISION_CONSTS_H
