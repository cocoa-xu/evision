import os
import re
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

import module_generator as MG
from module_generator import _guard_subsumes, _variant_shadowed
from emit.elixir.helpers import map_argtype_to_guard_elixir
from ir.functions import FuncInfo


class TestGuardSubsumes:
    def test_identical_struct_guard_is_subsumed(self):
        # Pattern 4: two GpuMat overloads emit the same struct guard.
        g = ['is_struct(__a0, Evision.CUDA.GpuMat)']
        assert _guard_subsumes(g, g) is True

    def test_is_number_subsumes_is_integer(self):
        # Pattern 3: a `double` clause (is_number) covers a later `int` clause.
        assert _guard_subsumes(['is_number(__a0)'], ['is_integer(__a0)']) is True

    def test_is_number_subsumes_is_float(self):
        assert _guard_subsumes(['is_number(__a0)'], ['is_float(__a0)']) is True

    def test_is_integer_does_not_subsume_is_number(self):
        # Reverse order: the is_number clause stays reachable for floats.
        assert _guard_subsumes(['is_integer(__a0)'], ['is_number(__a0)']) is False

    def test_is_integer_does_not_subsume_is_float(self):
        assert _guard_subsumes(['is_integer(__a0)'], ['is_float(__a0)']) is False

    def test_different_arity_never_subsumes(self):
        assert _guard_subsumes(['is_integer(__a0)'], ['is_integer(__a0)', 'is_binary(__a1)']) is False

    def test_multi_arg_subsumption_is_per_position(self):
        prior = ['is_binary(__a0)', 'is_number(__a1)']
        current = ['is_binary(__a0)', 'is_integer(__a1)']
        assert _guard_subsumes(prior, current) is True

    def test_multi_arg_mismatch_blocks_subsumption(self):
        prior = ['is_binary(__a0)', 'is_number(__a1)']
        current = ['is_integer(__a0)', 'is_integer(__a1)']
        assert _guard_subsumes(prior, current) is False

    def test_empty_guards_same_arity_subsume(self):
        # Two clauses with no guards and the same arity: the later is unreachable.
        assert _guard_subsumes([], []) is True

    def test_compound_mat_guard_is_not_a_bare_is_number_subsumer(self):
        # Safety: the Mat guard contains `is_number(...)` but must never subsume an
        # int clause, or a Mat+int overload pair would lose a reachable clause.
        mat_guard = ['(is_struct(__a0, Evision.Mat) or is_struct(__a0, Nx.Tensor) or is_number(__a0) or is_tuple(__a0))']
        assert _guard_subsumes(mat_guard, ['is_integer(__a0)']) is False

    def test_shorter_clause_does_not_subsume_longer(self):
        # Regression: a 1-arg Mat clause must not subsume a 2-arg (Mat, Vec3f) clause
        # whose 2nd arg is unguarded. Signatures of different length never subsume, so
        # the reachable longer clause is kept (this is why normalized_guard keeps an
        # entry per positional arg instead of dropping empty guards).
        mat = '(is_struct(__a0, Evision.Mat) or is_struct(__a0, Nx.Tensor) or is_number(__a0) or is_tuple(__a0))'
        assert _guard_subsumes([mat], [mat, '']) is False
        assert _guard_subsumes([mat, ''], [mat]) is False

    def test_identical_including_unguarded_positions_subsume(self):
        # Two clauses identical including an unguarded position: the later is dead.
        assert _guard_subsumes(['is_binary(__a0)', ''], ['is_binary(__a0)', '']) is True

    def test_unguarded_vs_guarded_position_blocks_subsumption(self):
        assert _guard_subsumes(['is_binary(__a0)', ''], ['is_binary(__a0)', 'is_integer(__a1)']) is False

    def test_guard_strings_match_subsumption_assumptions(self):
        # Ties the predicate to the real guard emitter so a future change there
        # cannot silently invalidate the is_number/is_integer/is_float assumptions.
        assert map_argtype_to_guard_elixir('__a0', 'double') == 'is_number(__a0)'
        assert map_argtype_to_guard_elixir('__a0', 'int') == 'is_integer(__a0)'
        assert map_argtype_to_guard_elixir('__a0', 'float') == 'is_float(__a0)'


GPUMAT = ['is_struct(__a0, Evision.CUDA.GpuMat)']


class TestVariantShadowed:
    def test_variant_with_opts_shadows_one_without(self):
        assert _variant_shadowed((GPUMAT, True), (GPUMAT, False)) is True

    def test_variant_without_opts_keeps_a_later_opts_clause(self):
        assert _variant_shadowed((GPUMAT, False), (GPUMAT, True)) is False

    def test_same_opts_shape_shadows(self):
        assert _variant_shadowed((GPUMAT, True), (GPUMAT, True)) is True
        assert _variant_shadowed((GPUMAT, False), (GPUMAT, False)) is True

    def test_is_number_with_opts_shadows_is_integer_without(self):
        assert _variant_shadowed((['is_number(__a0)'], True), (['is_integer(__a0)'], False)) is True

    def test_different_guards_never_shadow(self):
        assert _variant_shadowed((GPUMAT, True), (['is_tuple(__a0)'], False)) is False


def _gpumat_constructor():
    allocator = ['GpuMat_Allocator*', 'allocator', 'GpuMat::defaultAllocator()', []]
    func = FuncInfo('cuda_GpuMat', 'GpuMat', 'cv::cuda::GpuMat::GpuMat', True, 'cv.cuda', False)
    for args in (
        [['GpuMat', 'm', '', ['/C', '/Ref']]],
        [['Mat', 'arr', '', []], allocator],
        [['cuda::GpuMat', 'arr', '', []], allocator],
    ):
        func.add_variant(['cv.cuda.GpuMat.GpuMat', '', [], args, None, ''])
    return func


def _clause_heads(gen, name, arity):
    return [re.search(r'^\s*def (.*)$', code, re.M).group(1) for _, code in gen.function['elixir'][name][arity]]


def test_gpumat_copy_constructor_does_not_duplicate_the_input_array_clause(monkeypatch):
    # GpuMat(const GpuMat& m) and the GpuMat copy of GpuMat(InputArray arr, Allocator* = ...)
    # both emit gpuMat/1 guarded by is_struct(_, Evision.CUDA.GpuMat).
    monkeypatch.setattr(MG, 'unique_signatures', {})
    gen = MG.ModuleGenerator('CUDA.GpuMat')
    gen.gen_constructor('cv::cuda::GpuMat', 'cuda_GpuMat', _gpumat_constructor(), ['cuda', 'GpuMat'])

    gpumat_heads = [h for h in _clause_heads(gen, 'gpuMat', 1) if 'Evision.CUDA.GpuMat)' in h]
    assert gpumat_heads == ['gpuMat(arr) when is_struct(arr, Evision.CUDA.GpuMat)']
    assert any(h.startswith('gpuMat(arr, opts) when is_struct(arr, Evision.CUDA.GpuMat)')
               for h in _clause_heads(gen, 'gpuMat', 2))
