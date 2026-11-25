from helper import elixir_atom
from ir.functions import FuncInfo


def test_named_args_are_the_keys_the_nif_reads():
    func = FuncInfo('', 'reprojectImageTo3D', 'cv::reprojectImageTo3D', False, 'cv', False)
    func.add_variant(['cv.reprojectImageTo3D', 'void', [], [
        ['Mat', 'disparity', '', []],
        ['Mat', '_3dImage', '', ['/O']],
        ['Mat', 'Q', '', []],
        ['FileNode', 'fn', '', []],
    ], None, ''])
    keys = func.get_all_named_args()
    assert keys == ['disparity', '3dImage', 'q', 'func']
    assert [elixir_atom(k) for k in keys] == [':disparity', ':"3dImage"', ':q', ':func']
