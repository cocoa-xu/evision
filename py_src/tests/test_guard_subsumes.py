import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from module_generator import _guard_subsumes
from emit.elixir.helpers import map_argtype_to_guard_elixir


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
