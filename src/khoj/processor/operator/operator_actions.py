# --- Standardized Action Models ---
from typing import List, Literal, Optional, Union

from pydantic import BaseModel


class Point(BaseModel):
    x: int
    y: int


class BaseAction(BaseModel):
    type: str


class ClickAction(BaseAction):
    type: Literal["click"] = "click"
    x: int
    y: int
    button: Literal["left", "right", "middle", "wheel"] = "left"
    modifiers: str = None


class DoubleClickAction(BaseAction):
    type: Literal["double_click"] = "double_click"
    x: int
    y: int


class TripleClickAction(BaseAction):
    type: Literal["triple_click"] = "triple_click"
    x: int
    y: int


class ScrollAction(BaseAction):
    type: Literal["scroll"] = "scroll"
    x: Optional[int] = None
    y: Optional[int] = None
    scroll_x: Optional[int] = None
    scroll_y: Optional[int] = None
    scroll_direction: Optional[Literal["up", "down", "left", "right"]] = None
    scroll_amount: Optional[int] = 2


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
    x: int
    y: int


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
    CursorPositionAction,
    GotoAction,
    BackAction,
]
