"""Inheritance-flattening pass for emitted BEAM modules.

Elixir and Erlang have no class inheritance, so any inherited method that
the user might call on a derived class must be emitted under that derived
class's module. This pass walks each `ClassInfo`'s base chain (gated by
`config.inheritance_policy.BASE_CLASSES_TO_CHECK` plus a hand-tuned regex
heuristic) and copies the base's `methods` dict into the derived class's
methods map.

Currently flattens **methods only**. Properties and constants are not yet
flattened — that's a deliberate follow-up because adding them risks
name/arity collisions on the Elixir side (Elixir doesn't separate method
and accessor namespaces) and needs a per-collision audit.
"""

from config.inheritance_policy import BASE_CLASSES_TO_CHECK


def _should_flatten(base_class, current_class):
    """Encode the hand-curated policy: should we copy methods from
    `base_class` into `current_class`?

    The criterion is UX-driven, not technical: it lists classes where
    users routinely hold a `Derived` value and expect inherited methods
    to resolve through `Derived.method/N`. See feedback memory
    `feedback_inheritance_flattening`.
    """
    return bool(base_class) and (
        base_class in BASE_CLASSES_TO_CHECK
        or current_class.cname.startswith("cv::ml")
        or "Calibrate" in current_class.cname
        or "FaceRecognizer" in current_class.cname
        or "Facemark" in current_class.cname
        or "Collector" in current_class.cname
        or (current_class.base is not None and "Feature2D" in current_class.base)
        or (current_class.base is not None and "Matcher" in current_class.base)
        or (current_class.base is not None and "Algorithm" in current_class.base)
        or (current_class.base is not None and current_class.cname.startswith("cv::dnn"))
    )


def flatten_methods(class_info, codegen):
    """Return a (possibly enlarged) copy of `class_info.methods` that
    includes methods inherited from base classes per the policy above.

    The original `class_info.methods` dict is not mutated; the caller
    receives a fresh dict to feed to the emitter.
    """
    methods = class_info.methods.copy()
    base_class = class_info.base
    current_class = class_info

    while base_class is not None:
        if not _should_flatten(base_class, current_class):
            break
        if base_class not in codegen.classes or current_class.base is None:
            break
        base_class = codegen.classes[current_class.base]
        class_info._add_methods_from_class(base_class, methods)
        base_class, current_class = current_class.base, base_class

    return methods
