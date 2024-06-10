import evision/mat.{type Mat}
import evision/types.{type Void}

@external(erlang, "evision_highgui", "imshow")
pub fn imshow(win_name: String, mat: Mat) -> Void

@external(erlang, "evision_highgui", "waitKey")
pub fn wait_key(delay: Int) -> Void

@external(erlang, "evision_highgui", "destroyWindow")
pub fn destroy_window(win_name: String) -> Void

@external(erlang, "evision_highgui", "destroyAllWindows")
pub fn destroy_all_windows() -> Void
