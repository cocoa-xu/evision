import os
import sys
from types import SimpleNamespace

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from ir.inheritance import flatten_props  # noqa: E402


def _cls(cname, base, props):
    return SimpleNamespace(cname=cname, base=base, props=[SimpleNamespace(name=p) for p in props])


def _names(class_info, classes):
    return [p.name for p in flatten_props(class_info, SimpleNamespace(classes=classes))]


def test_dnn_layer_inherits_layer_info_props():
    info = _cls("cv::dnn::LayerInfo", None, ["blobs", "name", "type"])
    layer = _cls("cv::dnn::Layer", "dnn_LayerInfo", ["preferableTarget"])
    classes = {"dnn_LayerInfo": info, "dnn_Layer": layer}

    assert _names(layer, classes) == ["preferableTarget", "blobs", "name", "type"]


def test_own_prop_wins_over_inherited_one():
    info = _cls("cv::dnn::LayerInfo", None, ["name", "type"])
    layer = _cls("cv::dnn::Layer", "dnn_LayerInfo", ["name"])
    classes = {"dnn_LayerInfo": info, "dnn_Layer": layer}

    flattened = flatten_props(layer, SimpleNamespace(classes=classes))
    assert [p.name for p in flattened] == ["name", "type"]
    assert flattened[0] is layer.props[0]


def test_classes_outside_the_policy_keep_their_own_props():
    base = _cls("cv::Foo", None, ["x"])
    derived = _cls("cv::Bar", "Foo", ["y"])

    assert _names(derived, {"Foo": base, "Bar": derived}) == ["y"]


def test_props_come_from_every_ancestor_nearest_first():
    a = _cls("cv::dnn::A", None, ["a"])
    b = _cls("cv::dnn::B", "dnn_A", ["b"])
    c = _cls("cv::dnn::C", "dnn_B", ["c"])
    classes = {"dnn_A": a, "dnn_B": b, "dnn_C": c}

    assert _names(c, classes) == ["c", "b", "a"]


def test_walk_stops_at_an_unknown_ancestor():
    b = _cls("cv::dnn::B", "dnn_Missing", ["b"])
    c = _cls("cv::dnn::C", "dnn_B", ["c"])

    assert _names(c, {"dnn_B": b, "dnn_C": c}) == ["c", "b"]


def test_policy_is_checked_against_each_ancestor():
    grand = _cls("cv::Grand", None, ["g"])
    flow = _cls("cv::SparseOpticalFlow", "Grand", ["f"])
    derived = _cls("cv::Derived", "SparseOpticalFlow", ["d"])
    classes = {"Grand": grand, "SparseOpticalFlow": flow, "Derived": derived}

    assert _names(derived, classes) == ["d", "f"]
