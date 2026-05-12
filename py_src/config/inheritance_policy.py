"""Policy controlling which C++ base-class method sets are flattened into
their subclasses on the emitter side.

Elixir and Erlang have no inheritance, so a subclass's emitted module must
explicitly re-export any inherited method the user might want to call
through a subclass instance. This list (plus the regex predicates in
`class_info.py`) hand-picks the classes where flattening is wanted on
ergonomic grounds: cases where users routinely hold a `Derived` value and
expect `Derived.method/N` to resolve.

When OpenCV adds a new class whose users will want this behavior, add its
unqualified name here.
"""

BASE_CLASSES_TO_CHECK = [
    "GraphicalCodeDetector",
    "img_hash_ImgHashBase",
    "BackgroundSubtractor",
    "legacy_Tracker",
    "ml_StatModel",
    "phase_unwrapping_PhaseUnwrapping",
    "rapid_Tracker",
    "reg_Map",
    "reg_Mapper",
    "structured_light_StructuredLightPattern",
    "SparseOpticalFlow",
]
