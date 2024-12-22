import { getIconFromIconName } from "@/app/common/iconUtils";
import { ChatInputFocus } from "../chatInputArea/chatInputArea";

export enum SuggestionType {
    Paint = "Paint",
    Travel = "Travel",
    Health = "Health",
    Learning = "Learning",
    Math = "Mathematics",
    Language = "Language",
    PopCulture = "Pop Culture",
    Food = "Food",
    Interviewing = "Interviewing",
    Home = "Home",
    Fun = "Fun",
    Code = "Code",
    Finance = "Finance",
    Document = "Document",
    Image = "Image",
}

export interface StepOneSuggestion {
    type: SuggestionType;
    color: string;
    description: string;
    focus: ChatInputFocus;
}

export interface StepTwoSuggestion {
    prompt: string;
}

const suggestionToColorMap: { [key in SuggestionType]?: string } = {};

function addSuggestionColorMap(type: SuggestionType, color: string) {
    suggestionToColorMap[type] = color;
}

addSuggestionColorMap(SuggestionType.Paint, "indigo");
addSuggestionColorMap(SuggestionType.Travel, "yellow");
addSuggestionColorMap(SuggestionType.Health, "teal");
addSuggestionColorMap(SuggestionType.Learning, "purple");
addSuggestionColorMap(SuggestionType.Language, "blue");
addSuggestionColorMap(SuggestionType.PopCulture, "red");
addSuggestionColorMap(SuggestionType.Food, "yellow");
addSuggestionColorMap(SuggestionType.Interviewing, "orange");
addSuggestionColorMap(SuggestionType.Home, "green");
addSuggestionColorMap(SuggestionType.Fun, "fuchsia");
addSuggestionColorMap(SuggestionType.Code, "purple");
addSuggestionColorMap(SuggestionType.Finance, "green");
addSuggestionColorMap(SuggestionType.Math, "blue");
addSuggestionColorMap(SuggestionType.Image, "red");
addSuggestionColorMap(SuggestionType.Document, "orange");

const DEFAULT_COLOR = "orange";

export function convertSuggestionTitleToIconClass(title: string, color: string) {
    if (title === SuggestionType.Paint) return getIconFromIconName("Palette", color, "w-6", "h-6");
    if (title === SuggestionType.PopCulture)
        return getIconFromIconName("Confetti", color, "w-6", "h-6");
    if (title === SuggestionType.Travel) return getIconFromIconName("Jeep", color, "w-6", "h-6");
    if (title === SuggestionType.Learning) return getIconFromIconName("Book", color, "w-6", "h-6");
    if (title === SuggestionType.Health)
        return getIconFromIconName("Asclepius", color, "w-6", "h-6");
    if (title === SuggestionType.Fun) return getIconFromIconName("Island", color, "w-6", "h-6");
    if (title === SuggestionType.Home) return getIconFromIconName("House", color, "w-6", "h-6");
    if (title === SuggestionType.Language)
        return getIconFromIconName("Translate", color, "w-6", "h-6");
    if (title === SuggestionType.Code) return getIconFromIconName("Code", color, "w-6", "h-6");
    if (title === SuggestionType.Food) return getIconFromIconName("BowlFood", color, "w-6", "h-6");
    if (title === SuggestionType.Interviewing)
        return getIconFromIconName("Lectern", color, "w-6", "h-6");
    if (title === SuggestionType.Finance) return getIconFromIconName("Wallet", color, "w-6", "h-6");
    if (title === SuggestionType.Math)
        return getIconFromIconName("MathOperations", color, "w-6", "h-6");
    if (title === SuggestionType.Image) return getIconFromIconName("Image", color, "w-6", "h-6");
    else return getIconFromIconName("Lightbulb", color, "w-6", "h-6");
}

export const stepOneSuggestions: StepOneSuggestion[] = [
    {
        type: SuggestionType.Paint,
        description: "Create image",
        color: suggestionToColorMap[SuggestionType.Paint] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
    },
    {
        type: SuggestionType.Document,
        description: "Summarize text",
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        focus: ChatInputFocus.FILE,
    },
    {
        type: SuggestionType.Travel,
        description: "Find a place",
        color: suggestionToColorMap[SuggestionType.Travel] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
    },
    {
        type: SuggestionType.Language,
        description: "Translate text",
        color: suggestionToColorMap[SuggestionType.Language] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
    },
    {
        type: SuggestionType.PopCulture,
        description: "Find a movie",
        color: suggestionToColorMap[SuggestionType.PopCulture] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
    },
    {
        type: SuggestionType.Food,
        description: "Find a recipe",
        color: suggestionToColorMap[SuggestionType.Food] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
    },
    {
        type: SuggestionType.Interviewing,
        description: "Prepare for interview",
        color: suggestionToColorMap[SuggestionType.Interviewing] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
    },
    {
        type: SuggestionType.Fun,
        description: "Find a game",
        color: suggestionToColorMap[SuggestionType.Fun] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
    },
    {
        type: SuggestionType.Code,
        description: "Write code",
        color: suggestionToColorMap[SuggestionType.Code] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
    },
    {
        type: SuggestionType.Finance,
        description: "Create chart",
        color: suggestionToColorMap[SuggestionType.Finance] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
    },
    {
        type: SuggestionType.Math,
        description: "Solve a problem",
        color: suggestionToColorMap[SuggestionType.Math] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
    },
    {
        type: SuggestionType.Image,
        description: "Explain image",
        color: suggestionToColorMap[SuggestionType.Image] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
    },
    {
        type: SuggestionType.Learning,
        description: "Explain concept",
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
    },
];

export const stepTwoSuggestion: { [key: string]: StepTwoSuggestion[] } = {
    [SuggestionType.Paint]: [
        {
            prompt: "Paint a picture of a sunset but it's made of stained glass tiles.",
        },
        {
            prompt: "Paint a futuristic cityscape with flying cars.",
        },
        {
            prompt: "Paint a neon-lit street scene with reflections in the rain.",
        },
    ],
    [SuggestionType.Travel]: [
        {
            prompt: "Search for the best attractions in Austria Hungary.",
        },
        {
            prompt: "Find the top-rated coffee shops in Seattle.",
        },
    ],
    [SuggestionType.Health]: [
        {
            prompt: "Generate a weekly meal plan with recipes.",
        },
        {
            prompt: "Suggest healthy meal prep ideas for a busy work week.",
        },
    ],
    [SuggestionType.Learning]: [
        {
            prompt: "Research the history of the Eiffel Tower.",
        },
        {
            prompt: "Summarize the biography of this figure: https://en.wikipedia.org/wiki/Jean_Baptiste_Point_du_Sable",
        },
    ],
    [SuggestionType.Language]: [
        {
            prompt: "Translate the following text into Spanish: 'Hello, how are you?'",
        },
        {
            prompt: "Tell me how to greet someone in Arabic.",
        },
    ],
    [SuggestionType.PopCulture]: [
        {
            prompt: "Find the best-rated science fiction books of the last decade.",
        },
        {
            prompt: "Research the latest trends in K-pop and its global influence.",
        },
    ],
    [SuggestionType.Food]: [
        {
            prompt: "Find the best empanada recipe from Colombia.",
        },
        {
            prompt: "Suggest a healthy alternative to a popular fast food dish.",
        },
    ],
    [SuggestionType.Interviewing]: [
        {
            prompt: "Create interview prep questions for a consulting job.",
        },
        {
            prompt: "What information should I include in a CV for a PhD application?",
        },
    ],
    [SuggestionType.Home]: [
        {
            prompt: "Recommend plants that can grow well indoors.",
        },
        {
            prompt: "Suggest ways to reduce plastic waste in daily life.",
        },
    ],
    [SuggestionType.Fun]: [
        {
            prompt: "List creative date ideas for couples on a budget.",
        },
        {
            prompt: "Create a list of fun activities for a family game night.",
        },
    ],
    [SuggestionType.Code]: [
        {
            prompt: "Provide tips for writing an effective resume.",
        },
        {
            prompt: "Explain the concept of recursion with a simple coding example.",
        },
    ],
    [SuggestionType.Finance]: [
        {
            prompt: "Explain the concept of compound interest and its importance in long-term savings.",
        },
        {
            prompt: "Provide an overview of different types of retirement accounts (e.g., 401(k), IRA, Roth IRA).",
        },
    ],
    [SuggestionType.Math]: [
        {
            prompt: "Create a series of questions to help a student discover the principles of basic economics.",
        },
        {
            prompt: "Develop a hands-on experiment to demonstrate the concept of density to middle schoolers.",
        },
    ],
    [SuggestionType.Image]: [
        {
            prompt: "Explain what is happening in this photograph",
        },
        {
            prompt: "Show me how I can improve this UI design",
        },
    ],
    [SuggestionType.Document]: [
        {
            prompt: "Summarize the key concepts in this document.",
        },
        {
            prompt: "Provide a detailed explanation about the topic of this document.",
        },
    ],
};

export function getStepTwoSuggestions(type: string): StepTwoSuggestion[] {
    return stepTwoSuggestion[type] || [];
}
