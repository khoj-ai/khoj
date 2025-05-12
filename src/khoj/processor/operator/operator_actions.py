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
]
