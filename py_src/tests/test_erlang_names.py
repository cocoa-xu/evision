import pytest

from emit.erlang.helpers import map_uppercase_to_erlang_name


@pytest.mark.parametrize(
    "name,expected",
    [
        # all-caps acronym classes (folded fully)
        ("ALIKED_clear", "aliked_clear"),
        ("SIFT_create", "sift_create"),
        ("ORB_create", "orb_create"),
        # acronym prefix followed by a capitalized word (last capital starts the word)
        ("ANNIndex_addItems", "annIndex_addItems"),
        ("PCABackProject", "pcaBackProject"),
        ("HOGDescriptor_save", "hogDescriptor_save"),
        # single leading capital
        ("Canny", "canny"),
        # special case kept in namespace_map (not algorithmically derivable)
        ("SVDecomp", "svdDecomp"),
        # already lowercase, untouched
        ("add", "add"),
    ],
)
def test_map_uppercase_to_erlang_name(name, expected):
    assert map_uppercase_to_erlang_name(name) == expected
