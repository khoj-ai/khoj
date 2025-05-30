# --- Standardized Action Models ---
from typing import List, Literal, Optional, Union

from pydantic import BaseModel


class Point(BaseModel):
    x: float
    y: float


class BaseAction(BaseModel):
    type: str


class ClickAction(BaseAction):
    type: Literal["click"] = "click"
    x: float
    y: float
    button: Literal["left", "right", "middle", "wheel"] = "left"
    modifiers: str = None


class DoubleClickAction(BaseAction):
    type: Literal["double_click"] = "double_click"
    x: float
    y: float


class TripleClickAction(BaseAction):
    type: Literal["triple_click"] = "triple_click"
    x: float
    y: float


class ScrollAction(BaseAction):
    type: Literal["scroll"] = "scroll"
    x: Optional[float] = None
    y: Optional[float] = None
    scroll_x: Optional[float] = None
    scroll_y: Optional[float] = None
    scroll_direction: Optional[Literal["up", "down", "left", "right"]] = None
    scroll_amount: Optional[float] = 2.0


class KeypressAction(BaseAction):
    type: Literal["keypress"] = "keypress"
    keys: List[str]  # Standardized on list of keys


class TypeAction(BaseAction):
    type: Literal["type"] = "type"
    text: str


class WaitAction(BaseAction):
    type: Literal["wait"] = "wait"
    duration: float = 1.0


class ScreenshotAction(BaseAction):
    type: Literal["screenshot"] = "screenshot"


class MoveAction(BaseAction):
    type: Literal["move"] = "move"
    x: float
    y: float


class DragAction(BaseAction):
    type: Literal["drag"] = "drag"
    path: List[Point]


class MouseDownAction(BaseAction):
    type: Literal["mouse_down"] = "mouse_down"
    button: Literal["left", "right", "middle"] = "left"


class MouseUpAction(BaseAction):
    type: Literal["mouse_up"] = "mouse_up"
    button: Literal["left", "right", "middle"] = "left"


class HoldKeyAction(BaseAction):
    type: Literal["hold_key"] = "hold_key"
    text: str  # xdotool style key combination string
    duration: float = 1.0


class KeyUpAction(BaseAction):
    type: Literal["key_up"] = "key_up"
    key: str


class KeyDownAction(BaseAction):
    type: Literal["key_down"] = "key_down"
    key: str


class CursorPositionAction(BaseAction):
    type: Literal["cursor_position"] = "cursor_position"


class GotoAction(BaseAction):
    type: Literal["goto"] = "goto"
    url: str


class BackAction(BaseAction):
    type: Literal["back"] = "back"


class RequestUserAction(BaseAction):
    """Request user action to confirm or provide input."""

    type: Literal["request_user"] = "request_user"
    request: str


class NoopAction(BaseAction):
    """No operation action."""

    type: Literal["noop"] = "noop"


# --- Text Editor Actions ---
class TextEditorViewAction(BaseAction):
    """View contents of a file."""

    type: Literal["text_editor_view"] = "text_editor_view"
    path: str
    view_range: Optional[List[int]] = None  # [start_line, end_line]


class TextEditorCreateAction(BaseAction):
    """Create a new file with specified contents."""

    type: Literal["text_editor_create"] = "text_editor_create"
    path: str
    file_text: str


class TextEditorStrReplaceAction(BaseAction):
    """Execute an exact string match replacement on a file."""

    type: Literal["text_editor_str_replace"] = "text_editor_str_replace"
    path: str
    old_str: str
    new_str: str


class TextEditorInsertAction(BaseAction):
    """Insert new text after a specified line number."""

    type: Literal["text_editor_insert"] = "text_editor_insert"
    path: str
    insert_line: int
    new_str: str


class TerminalAction(BaseAction):
    """Insert new text after a specified line number."""

    type: Literal["terminal"] = "terminal"
    command: str
    restart: bool = False


OperatorAction = Union[
    ClickAction,
    DoubleClickAction,
    TripleClickAction,
    ScrollAction,
    KeypressAction,
    TypeAction,
    WaitAction,
    ScreenshotAction,
    MoveAction,
    DragAction,
    MouseDownAction,
    MouseUpAction,
    HoldKeyAction,
    KeyDownAction,
    KeyUpAction,
    CursorPositionAction,
    GotoAction,
    BackAction,
    RequestUserAction,
    NoopAction,
    TextEditorViewAction,
    TextEditorCreateAction,
    TextEditorStrReplaceAction,
    TextEditorInsertAction,
    TerminalAction,
]
