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
    actionTagline: string;
    focus: ChatInputFocus;
    intent: string;
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
addSuggestionColorMap(SuggestionType.Code, "teal");
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
    if (title === SuggestionType.Document) return getIconFromIconName("File", color, "w-6", "h-6");
    else return getIconFromIconName("Lightbulb", color, "w-6", "h-6");
}

export const stepOneSuggestions: StepOneSuggestion[] = [
    {
        type: SuggestionType.Document,
        actionTagline: "Summarize text",
        color: suggestionToColorMap[SuggestionType.Document] || DEFAULT_COLOR,
        focus: ChatInputFocus.FILE,
        intent: "Summarize this document",
    },
    {
        type: SuggestionType.Code,
        actionTagline: "Write code",
        color: suggestionToColorMap[SuggestionType.Code] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
        intent: "Write a program that",
    },
    {
        type: SuggestionType.Learning,
        actionTagline: "Explain concept",
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
        intent: "I want to understand a concept",
    },
    {
        type: SuggestionType.Paint,
        actionTagline: "Create image",
        color: suggestionToColorMap[SuggestionType.Paint] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
        intent: "Paint a picture of",
    },
    // {
    //     type: SuggestionType.Travel,
    //     actionTagline: "Find a place",
    //     color: suggestionToColorMap[SuggestionType.Travel] || DEFAULT_COLOR,
    //     focus: ChatInputFocus.MESSAGE,
    //     intent: "Find a place that is",
    // },
    {
        type: SuggestionType.Language,
        actionTagline: "Translate text",
        color: suggestionToColorMap[SuggestionType.Language] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
        intent: "Translate this text",
    },
    // {
    //     type: SuggestionType.PopCulture,
    //     actionTagline: "Explain a trend",
    //     color: suggestionToColorMap[SuggestionType.PopCulture] || DEFAULT_COLOR,
    //     focus: ChatInputFocus.MESSAGE,
    //     intent: "Tell me more about this phenomenon",
    // },
    // {
    //     type: SuggestionType.Food,
    //     actionTagline: "Find a recipe",
    //     color: suggestionToColorMap[SuggestionType.Food] || DEFAULT_COLOR,
    //     focus: ChatInputFocus.MESSAGE,
    //     intent: "Find a recipe for",
    // },
    // {
    //     type: SuggestionType.Interviewing,
    //     actionTagline: "Career advice",
    //     color: suggestionToColorMap[SuggestionType.Interviewing] || DEFAULT_COLOR,
    //     focus: ChatInputFocus.MESSAGE,
    //     intent: "Help me prepare for an interview",
    // },
    // {
    //     type: SuggestionType.Fun,
    //     actionTagline: "Get creative",
    //     color: suggestionToColorMap[SuggestionType.Fun] || DEFAULT_COLOR,
    //     focus: ChatInputFocus.MESSAGE,
    //     intent: "Suggest a fun activity",
    // },
    {
        type: SuggestionType.Finance,
        actionTagline: "Explain money",
        color: suggestionToColorMap[SuggestionType.Finance] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
        intent: "Help me build mental models for finance",
    },
    {
        type: SuggestionType.Math,
        actionTagline: "Explain math",
        color: suggestionToColorMap[SuggestionType.Math] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
        intent: "Help me understand the math behind",
    },
    {
        type: SuggestionType.Image,
        actionTagline: "Analyze image",
        color: suggestionToColorMap[SuggestionType.Image] || DEFAULT_COLOR,
        focus: ChatInputFocus.FILE,
        intent: "Explain the significance of this image",
    },
    {
        type: SuggestionType.Health,
        actionTagline: "Improve health",
        color: suggestionToColorMap[SuggestionType.Health] || DEFAULT_COLOR,
        focus: ChatInputFocus.MESSAGE,
        intent: "Help me improve my health",
    },
    // {
    //     type: SuggestionType.Home,
    //     actionTagline: "Improve home",
    //     color: suggestionToColorMap[SuggestionType.Home] || DEFAULT_COLOR,
    //     focus: ChatInputFocus.MESSAGE,
    //     intent: "Help me improve my home",
    // },
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
        {
            prompt: "Paint a portrait of a person with a unique hairstyle.",
        },
        {
            prompt: "Paint a landscape of a forest with a hidden waterfall.",
        },
    ],
    [SuggestionType.Travel]: [
        {
            prompt: "Search for the best attractions in Austria Hungary.",
        },
        {
            prompt: "Find the top-rated coffee shops in Seattle.",
        },
        {
            prompt: "Research the best hiking trails in the Swiss Alps.",
        },
        {
            prompt: "What is the best time of year to visit Bali?",
        },
        {
            prompt: "Find the best hidden gems in Nairobi.",
        },
    ],
    [SuggestionType.Health]: [
        {
            prompt: "Explain to me how to improve my posture.",
        },
        {
            prompt: "Tell me how what I eat affects my insulin levels.",
        },
        {
            prompt: "Suggest healthy meal prep ideas for a busy work week.",
        },
        {
            prompt: "Recommend good exercises to improve my flexibility.",
        },
        {
            prompt: "Explain the benefits of a plant-based diet",
        },
    ],
    [SuggestionType.Learning]: [
        {
            prompt: "Research the history of the Eiffel Tower.",
        },
        {
            prompt: "Summarize the biography of this figure: https://en.wikipedia.org/wiki/Jean_Baptiste_Point_du_Sable",
        },
        {
            prompt: "Explain the concept of 'machine learning' in simple terms.",
        },
        {
            prompt: "Find the best resources to learn about the history of the Roman Empire.",
        },
        {
            prompt: "Explain the concept of 'quantum entanglement' in simple terms.",
        },
    ],
    [SuggestionType.Language]: [
        {
            prompt: "Translate the following text into Spanish: 'Hello, how are you?'",
        },
        {
            prompt: "Tell me how to greet someone in Arabic.",
        },
        {
            prompt: "Explain the difference between the words 'ser' and 'estar' in Spanish.",
        },
        {
            prompt: "Translate the following text into French: 'Where is the nearest metro station?'",
        },
        {
            prompt: "Translate the following text into Japanese: 'I am learning Japanese.'",
        },
    ],
    [SuggestionType.PopCulture]: [
        {
            prompt: "Find the best-rated science fiction books of the last decade.",
        },
        {
            prompt: "Research the latest trends in K-pop and its global influence.",
        },
        {
            prompt: "Explain the plot of the movie 'Doctor Zhivago' in detail.",
        },
        {
            prompt: "What fashion is trending in jackets this season?",
        },
        {
            prompt: "Find the best indie movies of the last year",
        },
    ],
    [SuggestionType.Food]: [
        {
            prompt: "Find the best empanada recipe from Colombia.",
        },
        {
            prompt: "Suggest a healthy alternative to a popular fast food dish.",
        },
        {
            prompt: "Find the best recipe for a vegan chocolate cake.",
        },
        {
            prompt: "Suggest a recipe for a quick and easy weeknight dinner.",
        },
        {
            prompt: "Create a diagram that explains how to make a traditional Italian lasagna.",
        },
    ],
    [SuggestionType.Interviewing]: [
        {
            prompt: "Create interview prep questions for a consulting job.",
        },
        {
            prompt: "What information should I include in a CV for a PhD application?",
        },
        {
            prompt: "Suggest questions to ask during a job interview.",
        },
        {
            prompt: "What are the best ways to prepare for a technical interview?",
        },
        {
            prompt: "How can I improve my public speaking skills for an interview?",
        },
    ],
    [SuggestionType.Home]: [
        {
            prompt: "Recommend plants that can grow well indoors.",
        },
        {
            prompt: "Suggest ways to reduce plastic waste in daily life.",
        },
        {
            prompt: "Create a list of eco-friendly home cleaning products.",
        },
        {
            prompt: "Suggest ways to make a small apartment feel more spacious.",
        },
        {
            prompt: "Recommend ways to reduce energy consumption in a home.",
        },
    ],
    [SuggestionType.Fun]: [
        {
            prompt: "List creative date ideas for couples on a budget.",
        },
        {
            prompt: "Create a list of fun activities for a family game night.",
        },
        {
            prompt: "Give me three questions that can help me get to know someone better.",
        },
        {
            prompt: "Suggest fun activities for a group of friends on a rainy day.",
        },
        {
            prompt: "How can I improve my drawing skills?",
        },
    ],
    [SuggestionType.Code]: [
        {
            prompt: "Teach me how to write a simple 'Hello World' program in Python.",
        },
        {
            prompt: "Write a single page application using vanilla HTML, CSS, and JavaScript that displays a list of items.",
        },
        {
            prompt: "Explain the concept of recursion with a simple coding example.",
        },
        {
            prompt: "Create a simple calculator app using React.",
        },
        {
            prompt: "Write a function that takes an array of numbers and returns the sum of all the numbers.",
        },
    ],
    [SuggestionType.Finance]: [
        {
            prompt: "Create a chart to explain the concept of compound interest and its importance in long-term savings.",
        },
        {
            prompt: "Provide an overview of different types of retirement accounts (e.g., 401(k), IRA, Roth IRA).",
        },
        {
            prompt: "Explain the concept of diversification and its importance in investing.",
        },
        {
            prompt: "Create a budgeting plan for someone who wants to save money for a vacation.",
        },
        {
            prompt: "Explain the difference between a stock and a bond.",
        },
    ],
    [SuggestionType.Math]: [
        {
            prompt: "Create a series of questions to help a student discover the principles of basic economics.",
        },
        {
            prompt: "Develop a hands-on experiment to demonstrate the concept of density to middle schoolers.",
        },
        {
            prompt: "Explain the concept of a derivative and its applications in real life.",
        },
        {
            prompt: "Create a lesson plan to teach students about the Pythagorean theorem.",
        },
        {
            prompt: "Explain the concept of a limit in calculus and its importance in mathematics.",
        },
    ],
    [SuggestionType.Image]: [
        {
            prompt: "Explain what is happening in this photograph",
        },
        {
            prompt: "Show me how I can improve this UI design",
        },
        {
            prompt: "Explain the significance of this historical painting",
        },
        {
            prompt: "Can you explain this physics diagram to me?",
        },
        {
            prompt: "Explain this meme to me",
        },
    ],
    [SuggestionType.Document]: [
        {
            prompt: "Summarize the key concepts in this document.",
        },
        {
            prompt: "Provide a detailed explanation about the topic of this document.",
        },
        {
            prompt: "Create a visual representation of the information in this document.",
        },
        {
            prompt: "Find the main arguments in this document.",
        },
        {
            prompt: "Explain the relevance of this document to various other disciplines.",
        },
    ],
};

export function getStepTwoSuggestions(type: string): StepTwoSuggestion[] {
    return stepTwoSuggestion[type] || [];
}
