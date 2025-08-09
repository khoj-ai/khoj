# Source:
# https://github.com/xlang-ai/OSWorld/blob/main/run_uitars.py
# https://github.com/xlang-ai/OSWorld/blob/main/mm_agents/uitars_agent.py
# https://github.com/xlang-ai/OSWorld/blob/main/mm_agents/prompts.py#L1222
# https://github.com/xlang-ai/OSWorld/blob/main/lib_run_single.py

import ast
import base64
import logging
import math
import re
from io import BytesIO
from textwrap import dedent
from typing import Any, List

import numpy as np
from openai import AsyncAzureOpenAI, AsyncOpenAI
from openai.types.chat import ChatCompletion
from PIL import Image

from khoj.processor.operator.operator_actions import (
    BackAction,
    ClickAction,
    DoubleClickAction,
    DragAction,
    GotoAction,
    KeyDownAction,
    KeypressAction,
    KeyUpAction,
    MoveAction,
    OperatorAction,
    RequestUserAction,
    ScrollAction,
    TypeAction,
    WaitAction,
)
from khoj.processor.operator.operator_environment_base import EnvironmentType, EnvState
from khoj.utils.helpers import get_chat_usage_metrics

logger = logging.getLogger(__name__)


class GroundingAgentUitars:
    FINISH_WORD = "finished"
    WAIT_WORD = "wait"
    ENV_FAIL_WORD = "error_env"
    CALL_USER = "call_user"

    IMAGE_FACTOR = 28
    MIN_PIXELS = 100 * 28 * 28
    MAX_PIXELS = 16384 * 28 * 28
    MAX_RATIO = 200

    UITARS_NORMAL_ACTION_SPACE = dedent(
        """
    click(start_box='<|box_start|>(x1,y1)<|box_end|>')
    left_double(start_box='<|box_start|>(x1,y1)<|box_end|>')
    right_single(start_box='<|box_start|>(x1,y1)<|box_end|>')
    drag(start_box='<|box_start|>(x1,y1)<|box_end|>', end_box='<|box_start|>(x3,y3)<|box_end|>')
    hotkey(key='')
    type(content='') #If you want to submit your input, use "\\n" at the end of `content`.
    scroll(start_box='<|box_start|>(x1,y1)<|box_end|>', direction='down or up or right or left')
    wait() #Sleep for 5s and take a screenshot to check for any changes.
    finished(content='xxx') # Use escape characters \\', \\", and \\n in content part to ensure we can parse the content in normal python string format.
    """
    ).lstrip()

    def __init__(
        self,
        model_name: str,
        environment_type: EnvironmentType,
        client: AsyncOpenAI | AsyncAzureOpenAI,
        max_iterations=50,
        runtime_conf: dict = {
            "infer_mode": "qwen25vl_normal",
            "prompt_style": "qwen25vl_normal",
            "input_swap": True,
            "language": "English",
            "history_n": 5,
            "max_pixels": 16384 * 28 * 28,
            "min_pixels": 100 * 28 * 28,
            "callusr_tolerance": 3,
            "temperature": 0.0,
            "top_k": -1,
            "top_p": 0.9,
            "max_tokens": 500,
        },
        tracer: dict = None,
    ):
        self.model_name = model_name
        self.client = client
        self.tracer = tracer
        self.environment = environment_type

        self.max_iterations = max_iterations
        self.runtime_conf = runtime_conf
        self.temperature = self.runtime_conf["temperature"]
        self.top_k = self.runtime_conf["top_k"]
        self.top_p = self.runtime_conf["top_p"]
        self.max_tokens = self.runtime_conf["max_tokens"]
        self.infer_mode = self.runtime_conf["infer_mode"]
        self.prompt_style = self.runtime_conf["prompt_style"]
        self.input_swap = self.runtime_conf["input_swap"]
        self.language = self.runtime_conf["language"]
        self.max_pixels = self.runtime_conf["max_pixels"]
        self.min_pixels = self.runtime_conf["min_pixels"]
        self.callusr_tolerance = self.runtime_conf["callusr_tolerance"]

        self.thoughts: list[str] = []
        self.actions: list[list[OperatorAction]] = []
        self.observations: list[dict] = []
        self.history_images: list[bytes] = []
        self.history_responses: list[str] = []

        self.prompt_template = self.get_instruction(self.environment)
        self.prompt_action_space = self.UITARS_NORMAL_ACTION_SPACE

        if "history_n" in self.runtime_conf:
            self.history_n = self.runtime_conf["history_n"]
        else:
            self.history_n = 5

        self.cur_callusr_count = 0

    async def act(self, instruction: str, current_state: EnvState) -> tuple[str, list[OperatorAction]]:
        """
        Suggest the next action(s) based on the instruction and current environment.
        """
        messages = self._format_messages_for_api(instruction, current_state)

        recent_screenshot = Image.open(BytesIO(self.history_images[-1]))
        origin_resized_height = recent_screenshot.height
        origin_resized_width = recent_screenshot.width

        prediction, parsed_responses = self.parse_instruction_to_action(
            instruction, origin_resized_height, origin_resized_width
        )

        temperature = self.temperature
        try_times = 3
        while not parsed_responses:
            if try_times <= 0:
                logger.warning("Reach max retry times to fetch response from client, as error flag.")
                return "client error\nFAIL", []
            try:
                message_content = "\n".join([msg["content"][0].get("text") or "[image]" for msg in messages])
                logger.debug(f"User message content: {message_content}")
                response: ChatCompletion = await self.client.chat.completions.create(
                    model="ui-tars",
                    messages=messages,
                    frequency_penalty=1,
                    max_tokens=self.max_tokens,
                    temperature=temperature,
                    # top_k=top_k,
                    top_p=self.top_p,
                )
                prediction = response.choices[0].message.content.strip()
                self.tracer["usage"] = get_chat_usage_metrics(
                    self.model_name,
                    input_tokens=response.usage.prompt_tokens,
                    output_tokens=response.usage.completion_tokens,
                    usage=self.tracer["usage"],
                )
            except Exception as e:
                logger.debug(f"Error when fetching response from client, with error: {e}")
                prediction = None
                try_times -= 1

            try:
                parsed_responses = self.parse_action_to_structure_output(
                    prediction, origin_resized_height, origin_resized_width, self.max_pixels, self.min_pixels
                )
                break
            except Exception as e:
                logger.debug(f"Error when parsing response from client, with error: {e}")
                # If fail to parse the model response, we use sampling parameters to avoid it
                prediction = None
                try_times -= 1
                temperature = 1

        if prediction is None:
            return "client error\nFAIL", []

        self.history_responses.append(prediction)
        self.thoughts.append(prediction)

        try:
            parsed_responses = self.parse_action_to_structure_output(
                prediction, origin_resized_height, origin_resized_width, self.max_pixels, self.min_pixels
            )
        except Exception as e:
            print(f"Parsing action error: {prediction}, with error:\n{e}")
            return f"Parsing action error: {prediction}, with error:\n{e}\nFAIL", []

        return self._parse_action(parsed_responses, prediction)

    def _parse_action(self, parsed_responses: list[dict], prediction: str) -> tuple[str, list[OperatorAction]]:
        """
        Parse the model's prediction into actions and return the result.
        """
        actions: List[OperatorAction] = []
        last_image = Image.open(BytesIO(self.history_images[-1]))
        obs_image_height = last_image.height
        obs_image_width = last_image.width
        for parsed_response in parsed_responses:
            if parsed_response["action_type"] == self.FINISH_WORD:
                self.actions.append(actions)
                return f"{prediction}\nDONE", []

            elif parsed_response["action_type"] == self.WAIT_WORD:
                self.actions.append(actions)
                return prediction, [WaitAction(duration=3)]

            elif parsed_response["action_type"] == self.ENV_FAIL_WORD:
                self.actions.append(actions)
                return f"{prediction}\nFAIL", []

            elif parsed_response["action_type"] == self.CALL_USER:
                if self.callusr_tolerance > self.cur_callusr_count:
                    self.actions.append(actions)
                    self.cur_callusr_count += 1
                    return prediction, [RequestUserAction(request=parsed_response["text"])]
                else:
                    self.actions.append(actions)
                    return f"{prediction}\nFAIL", []

            actions.extend(
                self.parsing_response_to_action(parsed_response, obs_image_height, obs_image_width, self.input_swap)
            )

        self.actions.append(actions)

        if len(self.history_responses) >= self.max_iterations:
            # Default to FAIL if exceed max steps
            actions = []
            prediction = f"{prediction}\nFAIL"

        return prediction or "", actions

    def get_instruction(self, environment_type: EnvironmentType) -> str:
        """
        Get the instruction for the agent based on the environment type.
        """
        UITARS_COMPUTER_PREFIX_PROMPT = """
        You are a GUI agent. You are given a task and your action history, with screenshots. You need to perform the next action to complete the task.
        """
        UITARS_BROWSER_PREFIX_PROMPT = """
        You are a GUI agent. You are given a task and a screenshot of the web browser tab you operate. You need to perform the next action to complete the task.
        You control a single tab in a Chromium browser. You cannot access the OS, filesystem, the application window or the addressbar.
        """

        UITARS_USR_PROMPT_THOUGHT = """
        Try fulfill the user instruction to the best of your ability, especially when the instruction is given multiple times. Do not ignore the instruction.

        ## Output Format
        ```
        Thought: ...
        Action: ...
        ```

        ## Action Space
        {action_space}

        ## Note
        - Use {language} in `Thought` part.
        - Write a small plan and finally summarize your next action (with its target element) in one sentence in `Thought` part.

        ## User Instruction
        {instruction}
        """

        if environment_type == EnvironmentType.BROWSER:
            return dedent(UITARS_BROWSER_PREFIX_PROMPT + UITARS_USR_PROMPT_THOUGHT).lstrip()
        elif environment_type == EnvironmentType.COMPUTER:
            return dedent(UITARS_COMPUTER_PREFIX_PROMPT + UITARS_USR_PROMPT_THOUGHT).lstrip()
        else:
            raise ValueError(f"Unsupported environment type: {environment_type}")

    def _format_messages_for_api(self, instruction: str, current_state: EnvState):
        assert len(self.observations) == len(self.actions) and len(self.actions) == len(self.thoughts), (
            "The number of observations and actions should be the same."
        )

        self.history_images.append(base64.b64decode(current_state.screenshot))
        self.observations.append({"screenshot": current_state.screenshot, "accessibility_tree": None})

        user_prompt = self.prompt_template.format(
            instruction=instruction, action_space=self.prompt_action_space, language=self.language
        )

        if len(self.history_images) > self.history_n:
            self.history_images = self.history_images[-self.history_n :]

        messages: list[dict] = []
        images: list[Any] = []
        if isinstance(self.history_images, bytes):
            self.history_images = [self.history_images]
        elif isinstance(self.history_images, np.ndarray):
            self.history_images = list(self.history_images)
        elif isinstance(self.history_images, list):
            pass
        else:
            raise TypeError(f"Unidentified images type: {type(self.history_images)}")

        for _, image in enumerate(self.history_images):
            if len(images) >= self.history_n:
                break
            try:
                image = Image.open(BytesIO(image))
            except Exception as e:
                raise RuntimeError(f"Error opening image: {e}")

            if image.width * image.height > self.max_pixels:
                """
                Calculate a scaling factor to reduce the pixels in image to <= max_pixels if the image exceeds/is below the pixel limit,
                This scaling factor is calculated by taking the square root, ensuring the aspect ratio remains unchanged,
                so that the original relative coordinates can be reused directly without conversion.
                """
                resize_factor = math.sqrt(self.max_pixels / (image.width * image.height))
                width, height = int(image.width * resize_factor), int(image.height * resize_factor)
                image = image.resize((width, height))
            if image.width * image.height < self.min_pixels:
                resize_factor = math.sqrt(self.min_pixels / (image.width * image.height))
                width, height = math.ceil(image.width * resize_factor), math.ceil(image.height * resize_factor)
                image = image.resize((width, height))

            if image.mode != "RGB":
                image = image.convert("RGB")

            images.append(image)

        messages = [
            {"role": "system", "content": [{"type": "text", "text": "You are a helpful assistant."}]},
            {"role": "user", "content": [{"type": "text", "text": user_prompt}]},
        ]

        image_num = 0
        if len(self.history_responses) > 0:
            for history_idx, history_response in enumerate(self.history_responses):
                # send at most history_n images to the model
                if history_idx + self.history_n > len(self.history_responses):
                    cur_image = images[image_num]
                    encoded_string = self.pil_to_base64(cur_image)
                    messages.append(
                        {
                            "role": "user",
                            "content": [
                                {"type": "image_url", "image_url": {"url": f"data:image/png;base64,{encoded_string}"}}
                            ],
                        }
                    )
                    image_num += 1

                messages.append(
                    {"role": "assistant", "content": [{"type": "text", "text": self.add_box_token(history_response)}]}
                )

            cur_image = images[image_num]
            encoded_string = self.pil_to_base64(cur_image)
            messages.append(
                {
                    "role": "user",
                    "content": [{"type": "image_url", "image_url": {"url": f"data:image/png;base64,{encoded_string}"}}],
                }
            )
            image_num += 1

        else:
            cur_image = images[image_num]
            encoded_string = self.pil_to_base64(cur_image)
            messages.append(
                {
                    "role": "user",
                    "content": [{"type": "image_url", "image_url": {"url": f"data:image/png;base64,{encoded_string}"}}],
                }
            )
            image_num += 1

        return messages

    def reset(self):
        self.thoughts = []
        self.actions = []
        self.observations = []
        self.history_images = []
        self.history_responses = []

    # Define function to parse each action
    def parse_action_string(self, action_str):
        try:
            # Parse the string into an AST node
            node = ast.parse(action_str, mode="eval")

            # Ensure the node is an expression
            if not isinstance(node, ast.Expression):
                raise ValueError("Not an expression")

            # Get the body of the expression
            call = node.body

            # Ensure the body is a function call
            if not isinstance(call, ast.Call):
                raise ValueError("Not a function call")

            # Get the function name
            if isinstance(call.func, ast.Name):
                func_name = call.func.id
            elif isinstance(call.func, ast.Attribute):
                func_name = call.func.attr
            else:
                func_name = None

            # Get the keyword arguments
            kwargs = {}
            for kw in call.keywords:
                key = kw.arg
                # Handle different types of values, assuming they are all constants
                if isinstance(kw.value, ast.Constant):
                    value = kw.value.value
                elif isinstance(kw.value, ast.Str):  # Older Python compatibility
                    value = kw.value.s
                else:
                    value = None
                kwargs[key] = value

            return {"function": func_name, "args": kwargs}

        except Exception as e:
            print(f"Failed to parse action '{action_str}': {e}")
            return None

    def escape_single_quotes(self, text):
        # Match unescaped single quotes (not matching \')
        pattern = r"(?<!\\)'"
        return re.sub(pattern, r"\\'", text)

    def round_by_factor(self, number: int, factor: int) -> int:
        """Returns the closest integer to 'number' that is divisible by 'factor'."""
        return round(number / factor) * factor

    def ceil_by_factor(self, number: float, factor: int) -> int:
        """Returns the smallest integer greater than or equal to 'number' that is divisible by 'factor'."""
        return math.ceil(number / factor) * factor

    def floor_by_factor(self, number: float, factor: int) -> int:
        """Returns the largest integer less than or equal to 'number' that is divisible by 'factor'."""
        return math.floor(number / factor) * factor

    def smart_resize(
        self,
        height: int,
        width: int,
        factor: int = IMAGE_FACTOR,
        min_pixels: int = MIN_PIXELS,
        max_pixels: int = MAX_PIXELS,
    ) -> tuple[int, int]:
        """
        Rescales the image so that the following conditions are met:

        1. Both dimensions (height and width) are divisible by 'factor'.

        2. The total number of pixels is within the range ['min_pixels', 'max_pixels'].

        3. The aspect ratio of the image is maintained as closely as possible.
        """
        if max(height, width) / min(height, width) > self.MAX_RATIO:
            raise ValueError(
                f"absolute aspect ratio must be smaller than {self.MAX_RATIO}, got {max(height, width) / min(height, width)}"
            )
        h_bar = max(factor, self.round_by_factor(height, factor))
        w_bar = max(factor, self.round_by_factor(width, factor))
        if h_bar * w_bar > max_pixels:
            beta = math.sqrt((height * width) / max_pixels)
            h_bar = self.floor_by_factor(height / beta, factor)
            w_bar = self.floor_by_factor(width / beta, factor)
        elif h_bar * w_bar < min_pixels:
            beta = math.sqrt(min_pixels / (height * width))
            h_bar = self.ceil_by_factor(height * beta, factor)
            w_bar = self.ceil_by_factor(width * beta, factor)
        return h_bar, w_bar

    def parse_action_to_structure_output(
        self,
        text: str,
        origin_resized_height,
        origin_resized_width,
        max_pixels=16384 * 28 * 28,
        min_pixels=100 * 28 * 28,
    ):
        text = text.strip()
        smart_resize_height, smart_resize_width = self.smart_resize(
            origin_resized_height,
            origin_resized_width,
            factor=self.IMAGE_FACTOR,
            min_pixels=min_pixels,
            max_pixels=max_pixels,
        )

        # Regular expression to match Action string
        if text.startswith("Thought:"):
            thought_pattern = r"Thought: (.+?)(?=\s*Action:|$)"
        elif text.startswith("Reflection:"):
            thought_pattern = r"Reflection: (.+?)Action_Summary: (.+?)(?=\s*Action:|$)"
        elif text.startswith("Action_Summary:"):
            thought_pattern = r"Action_Summary: (.+?)(?=\s*Action:|$)"
        else:
            thought_pattern = r"Thought: (.+?)(?=\s*Action:|$)"
        reflection, thought = None, None
        thought_match = re.search(thought_pattern, text, re.DOTALL)
        if thought_match:
            if len(thought_match.groups()) == 1:
                thought = thought_match.group(1).strip()
            elif len(thought_match.groups()) == 2:
                thought = thought_match.group(2).strip()
                reflection = thought_match.group(1).strip()
        assert "Action:" in text
        action_str = text.split("Action:")[-1]

        tmp_all_action = action_str.split("\n\n")
        all_action = []
        for action_str in tmp_all_action:
            if "type(content" in action_str:
                # Regex to match string in content and escape single quotes
                def escape_quotes(match):
                    content = match.group(1)
                    return content

                # Use regex to replace
                pattern = r"type\(content='(.*?)'\)"  # Match type(content='...')
                content = re.sub(pattern, escape_quotes, action_str)

                # Process the string
                action_str = self.escape_single_quotes(content)
                action_str = "type(content='" + action_str + "')"
            all_action.append(action_str)

        parsed_actions = [self.parse_action_string(action.replace("\n", "\\n").lstrip()) for action in all_action]
        actions: list[dict] = []
        for action_instance, raw_str in zip(parsed_actions, all_action):
            if action_instance is None:
                print(f"Action can't parse: {raw_str}")
                raise ValueError(f"Action can't parse: {raw_str}")
            action_type = action_instance["function"]
            params = action_instance["args"]

            action_inputs = {}
            for param_name, param in params.items():
                if param == "":
                    continue
                param = param.lstrip().rstrip()  # Remove quotes, extra spaces
                # Process start_box, end_box parameter format '<bbox>x1 y1 x2 y2</bbox>'
                action_inputs[param_name.strip()] = param

                if "start_box" in param_name or "end_box" in param_name:
                    ori_box = param
                    # Remove parentheses and split the string by commas
                    numbers = ori_box.replace("(", "").replace(")", "").split(",")

                    # Convert to float and scale by 1000 as output is absolute coordinates
                    float_numbers = []
                    for num_idx, num in enumerate(numbers):
                        num = float(num)
                        if (num_idx + 1) % 2 == 0:
                            float_numbers.append(float(num / smart_resize_height))
                        else:
                            float_numbers.append(float(num / smart_resize_width))

                    if len(float_numbers) == 2:
                        float_numbers = [float_numbers[0], float_numbers[1], float_numbers[0], float_numbers[1]]
                    action_inputs[param_name.strip()] = str(float_numbers)

            actions.append(
                {
                    "reflection": reflection,
                    "thought": thought,
                    "action_type": action_type,
                    "action_inputs": action_inputs,
                    "text": text,
                }
            )
        return actions

    def parsing_response_to_action(
        self, responses, image_height: int, image_width: int, input_swap: bool = True
    ) -> List[OperatorAction]:
        """
        Parses the output of the M model into actions in OSWorld and generates a pyautogui code string.
        Parameters:
            response: A dictionary containing the model's output, structured like:
            {
                "action_type": "hotkey",
                "action_inputs": {
                    "hotkey": "v ctrl",
                    "start_box": None,
                    "end_box": None
                }
            }
        Returns:
            The generated operator actions list
        """

        actions: List[OperatorAction] = []
        if isinstance(responses, dict):
            responses = [responses]
        for response_id, response in enumerate(responses):
            if "observation" in response:
                observation = response["observation"]
            else:
                observation = ""

            if "thought" in response:
                thought = response["thought"]
            else:
                thought = ""

            if response_id == 0:
                logger.debug(f"UITars Grounder:\nObservation:{observation}\nThought:\n{thought}")
            else:
                actions.append(WaitAction(duration=1))

            action_dict = response
            action_type = action_dict.get("action_type")
            action_inputs = action_dict.get("action_inputs", {})

            if action_type in ["hotkey", "press"]:
                # Parsing hotkey action
                if "key" in action_inputs:
                    hotkey = action_inputs.get("key", "")
                else:
                    hotkey = action_inputs.get("hotkey", "")

                if hotkey == "arrowleft":
                    hotkey = "left"

                elif hotkey == "arrowright":
                    hotkey = "right"

                elif hotkey == "arrowup":
                    hotkey = "up"

                elif hotkey == "arrowdown":
                    hotkey = "down"

                if hotkey:
                    # Handle other hotkeys
                    keys = hotkey.split()  # Split the keys by space
                    key_combination = []
                    for key in keys:
                        if key == "space":
                            key = " "
                        key_combination.append(key)
                    actions.append(KeypressAction(keys=key_combination))

            elif action_type == "keyup":
                key_to_up = action_inputs.get("key", "")
                actions.append(KeyUpAction(key=key_to_up))

            elif action_type == "keydown":
                key_to_down = action_inputs.get("key", "")
                actions.append(KeyDownAction(key=key_to_down))

            elif action_type == "type":
                # Parsing typing action using clipboard
                content = action_inputs.get("content", "")
                content = self.escape_single_quotes(content)
                stripped_content = content
                if content.endswith("\n") or content.endswith("\\n"):
                    stripped_content = stripped_content.rstrip("\\n").rstrip("\n")
                if content:
                    if input_swap:
                        # ignore copying text to clipboard for now
                        pass
                    actions.append(TypeAction(text=stripped_content))
                    if content.endswith("\n") or content.endswith("\\n"):
                        actions.append(KeypressAction(keys=["enter"]))

            elif action_type in ["drag", "select"]:
                # Parsing drag or select action based on start and end_boxes
                start_box = action_inputs.get("start_box")
                end_box = action_inputs.get("end_box")
                if start_box and end_box:
                    x1, y1, x2, y2 = eval(start_box)  # Assuming box is in [x1, y1, x2, y2]
                    sx = round(float((x1 + x2) / 2) * image_width, 3)
                    sy = round(float((y1 + y2) / 2) * image_height, 3)
                    x1, y1, x2, y2 = eval(end_box)  # Assuming box is in [x1, y1, x2, y2]
                    ex = round(float((x1 + x2) / 2) * image_width, 3)
                    ey = round(float((y1 + y2) / 2) * image_height, 3)
                    actions.append(MoveAction(x=sx, y=sy))
                    actions.append(DragAction(path=[(sx, sy), (ex, ey)]))

            elif action_type == "scroll":
                # Parsing scroll action
                start_box = action_inputs.get("start_box")
                if start_box:
                    x1, y1, x2, y2 = eval(start_box)  # Assuming box is in [x1, y1, x2, y2]
                    x = round(float((x1 + x2) / 2) * image_width, 3)
                    y = round(float((y1 + y2) / 2) * image_height, 3)

                    # First click the element, then scroll
                    # actions.append(ClickAction(x=x, y=y, button='left'))
                else:
                    x = None
                    y = None
                direction = action_inputs.get("direction", "down")

                if "up" in direction.lower():
                    actions.append(ScrollAction(amount=5, scroll_direction="up", x=x, y=y))
                elif "left" in direction.lower():
                    actions.append(ScrollAction(amount=5, scroll_direction="left", x=x, y=y))
                elif "right" in direction.lower():
                    actions.append(ScrollAction(amount=5, scroll_direction="right", x=x, y=y))
                else:
                    actions.append(ScrollAction(amount=5, scroll_direction="down", x=x, y=y))

            elif action_type in ["click", "left_single", "left_double", "right_single", "hover"]:
                # Parsing mouse click actions
                start_box = action_inputs.get("start_box")
                start_box = str(start_box)
                if start_box:
                    start_box = eval(start_box)
                    if len(start_box) == 4:
                        x1, y1, x2, y2 = start_box  # Assuming box is in [x1, y1, x2, y2]
                    elif len(start_box) == 2:
                        x1, y1 = start_box
                        x2 = x1
                        y2 = y1
                    x = round(float((x1 + x2) / 2) * image_width, 3)
                    y = round(float((y1 + y2) / 2) * image_height, 3)
                    if action_type == "left_single" or action_type == "click":
                        actions.append(ClickAction(x=x, y=y, button="left"))
                    elif action_type == "left_double":
                        actions.append(DoubleClickAction(x=x, y=y, button="left"))
                    elif action_type == "right_single":
                        actions.append(ClickAction(x=x, y=y, button="right"))
                    elif action_type == "hover":
                        actions.append(MoveAction(x=x, y=y))

            elif action_type == "goto":
                url = action_inputs.get("url", "")
                actions.append(GotoAction(url=url))

            elif action_type == "back":
                actions.append(BackAction())

            elif action_type in ["finished"]:
                actions = []

            else:
                logger.error(f"\n# Unrecognized action type: {action_type}")

        return actions

    def parsing_response_to_pyautogui_code(
        self, responses, image_height: int, image_width: int, input_swap: bool = True
    ) -> str:
        """
        Parses model suggested actions for the GUI environment and generates the pyautogui code string to run.
        Parameters:
            response: A dictionary containing the model's output, structured like:
            {
                "action_type": "hotkey",
                "action_inputs": {
                    "hotkey": "v ctrl",
                    "start_box": None,
                    "end_box": None
                }
            }
        Returns:
            The pyautogui code string
        """

        pyautogui_code = "import pyautogui\nimport time\n"
        actions = []
        if isinstance(responses, dict):
            responses = [responses]
        for response_id, response in enumerate(responses):
            if "observation" in response:
                observation = response["observation"]
            else:
                observation = ""

            if "thought" in response:
                thought = response["thought"]
            else:
                thought = ""

            if response_id == 0:
                pyautogui_code += f"'''\nObservation:\n{observation}\n\nThought:\n{thought}\n'''\n"
            else:
                pyautogui_code += "\ntime.sleep(1)\n"

            action_dict = response
            action_type = action_dict.get("action_type")
            action_inputs = action_dict.get("action_inputs", {})

            if action_type == "hotkey":
                # Parsing hotkey action
                if "key" in action_inputs:
                    hotkey = action_inputs.get("key", "")
                else:
                    hotkey = action_inputs.get("hotkey", "")

                if hotkey == "arrowleft":
                    hotkey = "left"

                elif hotkey == "arrowright":
                    hotkey = "right"

                elif hotkey == "arrowup":
                    hotkey = "up"

                elif hotkey == "arrowdown":
                    hotkey = "down"

                if hotkey:
                    actions.append(KeypressAction(keys=[hotkey]))

            elif action_type == "press":
                # Parsing press action
                if "key" in action_inputs:
                    key_to_press = action_inputs.get("key", "")
                else:
                    key_to_press = action_inputs.get("press", "")

                if hotkey == "arrowleft":
                    hotkey = "left"

                elif hotkey == "arrowright":
                    hotkey = "right"

                elif hotkey == "arrowup":
                    hotkey = "up"

                elif hotkey == "arrowdown":
                    hotkey = "down"

                elif hotkey == "space":
                    hotkey = " "

                if key_to_press:
                    # Simulate pressing a single key
                    pyautogui_code += f"\npyautogui.press({repr(key_to_press)})"

            elif action_type == "keyup":
                key_to_up = action_inputs.get("key", "")
                pyautogui_code += f"\npyautogui.keyUp({repr(key_to_up)})"

            elif action_type == "keydown":
                key_to_down = action_inputs.get("key", "")
                pyautogui_code += f"\npyautogui.keyDown({repr(key_to_down)})"

            elif action_type == "type":
                # Parsing typing action using clipboard
                content = action_inputs.get("content", "")
                content = self.escape_single_quotes(content)
                stripped_content = content
                if content.endswith("\n") or content.endswith("\\n"):
                    stripped_content = stripped_content.rstrip("\\n").rstrip("\n")
                if content:
                    if input_swap:
                        actions += TypeAction()
                        pyautogui_code += "\nimport pyperclip"
                        pyautogui_code += f"\npyperclip.copy('{stripped_content}')"
                        pyautogui_code += "\npyautogui.hotkey('ctrl', 'v')"
                        pyautogui_code += "\ntime.sleep(0.5)\n"
                        if content.endswith("\n") or content.endswith("\\n"):
                            pyautogui_code += "\npyautogui.press('enter')"
                    else:
                        pyautogui_code += f"\npyautogui.write('{stripped_content}', interval=0.1)"
                        pyautogui_code += "\ntime.sleep(0.5)\n"
                        if content.endswith("\n") or content.endswith("\\n"):
                            pyautogui_code += "\npyautogui.press('enter')"

            elif action_type in ["drag", "select"]:
                # Parsing drag or select action based on start and end_boxes
                start_box = action_inputs.get("start_box")
                end_box = action_inputs.get("end_box")
                if start_box and end_box:
                    x1, y1, x2, y2 = eval(start_box)  # Assuming box is in [x1, y1, x2, y2]
                    sx = round(float((x1 + x2) / 2) * image_width, 3)
                    sy = round(float((y1 + y2) / 2) * image_height, 3)
                    x1, y1, x2, y2 = eval(end_box)  # Assuming box is in [x1, y1, x2, y2]
                    ex = round(float((x1 + x2) / 2) * image_width, 3)
                    ey = round(float((y1 + y2) / 2) * image_height, 3)
                    pyautogui_code += f"\npyautogui.moveTo({sx}, {sy})\n\npyautogui.dragTo({ex}, {ey}, duration=1.0)\n"

            elif action_type == "scroll":
                # Parsing scroll action
                start_box = action_inputs.get("start_box")
                if start_box:
                    x1, y1, x2, y2 = eval(start_box)  # Assuming box is in [x1, y1, x2, y2]
                    x = round(float((x1 + x2) / 2) * image_width, 3)
                    y = round(float((y1 + y2) / 2) * image_height, 3)

                    # First click the element, then scroll
                    # pyautogui_code += f"\npyautogui.click({x}, {y}, button='left')"
                else:
                    x = None
                    y = None
                direction = action_inputs.get("direction", "")

                if x is None:
                    if "up" in direction.lower():
                        pyautogui_code += "\npyautogui.scroll(5)"
                    elif "down" in direction.lower():
                        pyautogui_code += "\npyautogui.scroll(-5)"
                else:
                    if "up" in direction.lower():
                        pyautogui_code += f"\npyautogui.scroll(5, x={x}, y={y})"
                    elif "down" in direction.lower():
                        pyautogui_code += f"\npyautogui.scroll(-5, x={x}, y={y})"

            elif action_type in ["click", "left_single", "left_double", "right_single", "hover"]:
                # Parsing mouse click actions
                start_box = action_inputs.get("start_box")
                start_box = str(start_box)
                if start_box:
                    start_box = eval(start_box)
                    if len(start_box) == 4:
                        x1, y1, x2, y2 = start_box  # Assuming box is in [x1, y1, x2, y2]
                    elif len(start_box) == 2:
                        x1, y1 = start_box
                        x2 = x1
                        y2 = y1
                    x = round(float((x1 + x2) / 2) * image_width, 3)
                    y = round(float((y1 + y2) / 2) * image_height, 3)
                    if action_type == "left_single" or action_type == "click":
                        pyautogui_code += f"\npyautogui.click({x}, {y}, button='left')"
                    elif action_type == "left_double":
                        pyautogui_code += f"\npyautogui.doubleClick({x}, {y}, button='left')"
                    elif action_type == "right_single":
                        pyautogui_code += f"\npyautogui.click({x}, {y}, button='right')"
                    elif action_type == "hover":
                        pyautogui_code += f"\npyautogui.moveTo({x}, {y})"

            elif action_type in ["finished"]:
                pyautogui_code = "DONE"

            else:
                pyautogui_code += f"\n# Unrecognized action type: {action_type}"

        return pyautogui_code

    def parse_instruction_to_action(
        self, instruction: str, origin_resized_height: int, origin_resized_width: int
    ) -> tuple[str, list[dict]]:
        """
        Parse instruction into action with simple string match for GOTO and BACK actions.

        Useful for actions that do not need to invoke the visual grounding model.
        """
        prediction, parsed_responses = None, []
        # handle GOTO <URL>, BACK actions at the end of the response.
        if instruction.strip().splitlines()[-1].strip().startswith("GOTO"):
            url = instruction.split("GOTO")[-1].strip()
            prediction = f"Thought: Let me go to {url}\nAction: goto(url='{url}')"
            parsed_responses = self.parse_action_to_structure_output(
                prediction, origin_resized_height, origin_resized_width, self.max_pixels, self.min_pixels
            )
        elif instruction.strip().endswith("BACK"):
            prediction = "Thought: Let me go back to the previous page.\nAction: back()"
            parsed_responses = self.parse_action_to_structure_output(
                prediction, origin_resized_height, origin_resized_width, self.max_pixels, self.min_pixels
            )
        return prediction, parsed_responses

    def add_box_token(self, input_string):
        # Step 1: Split the string into individual actions
        if "Action: " in input_string and "start_box=" in input_string:
            suffix = input_string.split("Action: ")[0] + "Action: "
            actions = input_string.split("Action: ")[1:]
            processed_actions = []
            for action in actions:
                action = action.strip()
                # Step 2: Extract coordinates (start_box or end_box) using regex
                coordinates = re.findall(r"(start_box|end_box)='\((\d+),\s*(\d+)\)'", action)

                updated_action = action  # Start with the original action
                for coord_type, x, y in coordinates:
                    # Convert x and y to integers
                    updated_action = updated_action.replace(
                        f"{coord_type}='({x},{y})'", f"{coord_type}='<|box_start|>({x},{y})<|box_end|>'"
                    )
                processed_actions.append(updated_action)

            # Step 5: Reconstruct the final string
            final_string = suffix + "\n\n".join(processed_actions)
        else:
            final_string = input_string
        return final_string

    def pil_to_base64(self, image):
        buffer = BytesIO()
        image.save(buffer, format="PNG")
        return base64.b64encode(buffer.getvalue()).decode("utf-8")
