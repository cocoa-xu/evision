#!/usr/bin/env python3
# -*- coding: utf-8 -*-


def evision_elixir_fixes(): 
    return [
        """
    @spec imdecode(binary(), integer()) :: Evision.Mat.maybe_mat_out()
    def imdecode(buf, flags) when is_integer(flags)
    do
        positional = [
            buf: buf,
            flags: flags
        ]
        :evision_nif.imdecode(positional)
        |> Evision.Internal.Structurise.to_struct()
    end
"""
    ]


def evision_erlang_fixes():
    return [
        """
imdecode(Buf, Flags) ->
  Ret = evision_nif:imdecode([{buf, Buf}, {flags, Flags}]),
  evision_internal_structurise:to_struct(Ret).
"""
    ]
