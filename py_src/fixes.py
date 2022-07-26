#!/usr/bin/env python3
# -*- coding: utf-8 -*-


def evision_elixir_fixes(): 
    return [
        """
    @doc namespace: :cv
    def imdecode(buf, flags) when is_integer(flags)
    do
        positional = [
        buf: buf,
        flags: flags
        ]
        :evision_nif.imdecode(positional)
    end
    deferror imdecode(buf, flags)
"""
    ]


def evision_erlang_fixes():
    return [
        """
imdecode(Buf, Flags) ->
  evision:imdecode([{buf, Buf}, {flags, Flags}]).
"""
    ]
