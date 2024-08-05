export interface Suggestion {
    type: string;
    color: string;
    description: string;
    link: string;
}

enum SuggestionType {
    Automation = "Automation",
    Paint = "Paint",
    Travel = "Travel",
    Health = "Health",
    Learning = "Learning",
    Language = "Language",
    PopCulture = "Pop Culture",
    Food = "Food",
    Interviewing = "Interviewing",
    Home = "Home",
    Fun = "Fun",
    Code = "Code",
}

const suggestionToColorMap: { [key in SuggestionType]?: string } = {};

function addSuggestionColorMap(type: SuggestionType, color: string) {
    suggestionToColorMap[type] = color;
}

addSuggestionColorMap(SuggestionType.Automation, "blue");
addSuggestionColorMap(SuggestionType.Paint, "green");
addSuggestionColorMap(SuggestionType.Travel, "yellow");
addSuggestionColorMap(SuggestionType.Health, "orange");
addSuggestionColorMap(SuggestionType.Learning, "purple");
addSuggestionColorMap(SuggestionType.Language, "blue");
addSuggestionColorMap(SuggestionType.PopCulture, "red");
addSuggestionColorMap(SuggestionType.Food, "yellow");
addSuggestionColorMap(SuggestionType.Interviewing, "purple");
addSuggestionColorMap(SuggestionType.Home, "green");
addSuggestionColorMap(SuggestionType.Fun, "fuschia");
addSuggestionColorMap(SuggestionType.Code, "purple");

const DEFAULT_COLOR = "orange";


export const suggestionsData: Suggestion[] = [
    {
        type: SuggestionType.Automation,
        color: suggestionToColorMap[SuggestionType.Automation] || DEFAULT_COLOR,
        description: "Send me a summary of HackerNews every morning.",
        link: "/automations?subject=Summarizing%20Top%20Headlines%20from%20HackerNews&query=Summarize%20the%20top%20headlines%20on%20HackerNews&crontime=00%207%20*%20*%20*",
    },
    {
        type: SuggestionType.Automation,
        color: suggestionToColorMap[SuggestionType.Automation] || DEFAULT_COLOR,
        description: "Compose a bedtime story that a five-year-old might enjoy.",
        link: "/automations?subject=Daily%20Bedtime%20Story&query=Compose%20a%20bedtime%20story%20that%20a%20five-year-old%20might%20enjoy.%20It%20should%20not%20exceed%20five%20paragraphs.%20Appeal%20to%20the%20imagination%2C%20but%20weave%20in%20learnings.&crontime=0%2021%20*%20*%20*",
    },
    {
        type: SuggestionType.Paint,
        color: suggestionToColorMap[SuggestionType.Paint] || DEFAULT_COLOR,
        description: "Paint a picture of a sunset but it's made of stained glass tiles",
        link: "",
    },
    {
        type: SuggestionType.Travel,
        color: suggestionToColorMap[SuggestionType.Travel] || DEFAULT_COLOR,
        description: "Search for the best attractions in Austria Hungary",
        link: "",
    },
    {
        type: SuggestionType.Health,
        color: suggestionToColorMap[SuggestionType.Health] || DEFAULT_COLOR,
        description: "Generate a weekly meal plan with recipes.",
        link: "/automations?subject=Weekly Meal Plan&query=Create a weekly meal plan with 7 dinner recipes, including ingredients and brief instructions. Focus on balanced, healthy meals.&crontime=0 18 * * 0",
    },
    {
        type: SuggestionType.Paint,
        color: suggestionToColorMap[SuggestionType.Paint] || DEFAULT_COLOR,
        description: "Paint a futuristic cityscape with flying cars.",
        link: "",
    },
    {
        type: SuggestionType.Travel,
        color: suggestionToColorMap[SuggestionType.Travel] || DEFAULT_COLOR,
        description: "Find the top-rated coffee shops in Seattle.",
        link: "",
    },
    {
        type: SuggestionType.Automation,
        color: suggestionToColorMap[SuggestionType.Automation] || DEFAULT_COLOR,
        description: "Send daily motivational quotes.",
        link: "/automations?subject=Daily Motivation&query=Provide an inspiring quote for the day along with a brief explanation of its meaning.&crontime=0 7 * * *",
    },
    {
        type: SuggestionType.Paint,
        color: suggestionToColorMap[SuggestionType.Paint] || DEFAULT_COLOR,
        description: "Create an abstract representation of jazz music.",
        link: "",
    },
    {
        type: SuggestionType.Learning,
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        description: "Research the history of the Eiffel Tower.",
        link: "",
    },
    {
        type: SuggestionType.Automation,
        color: suggestionToColorMap[SuggestionType.Automation] || DEFAULT_COLOR,
        description: "Compile a weekly news summary.",
        link: "/automations?subject=Weekly News Digest&query=Summarize the top 5 most important news stories of the week across various categories.&crontime=0 18 * * 5",
    },
    {
        type: SuggestionType.Paint,
        color: suggestionToColorMap[SuggestionType.Paint] || DEFAULT_COLOR,
        description: "Paint a portrait of a cat wearing a Victorian-era costume.",
        link: "",
    },
    {
        type: SuggestionType.Travel,
        color: suggestionToColorMap[SuggestionType.Travel] || DEFAULT_COLOR,
        description: "Find beginner-friendly hiking trails near Los Angeles.",
        link: "",
    },
    {
        type: SuggestionType.Automation,
        color: suggestionToColorMap[SuggestionType.Automation] || DEFAULT_COLOR,
        description: "Generate a daily writing prompt.",
        link: "/automations?subject=Daily Writing Prompt&query=Create an engaging writing prompt suitable for short story or journal writing.&crontime=0 9 * * *",
    },
    {
        type: SuggestionType.Paint,
        color: suggestionToColorMap[SuggestionType.Paint] || DEFAULT_COLOR,
        description: "Create a surrealist landscape inspired by Salvador Dali.",
        link: "",
    },
    {
        type: SuggestionType.Learning,
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        description: "Research the benefits and drawbacks of electric vehicles.",
        link: "",
    },
    {
        type: SuggestionType.Automation,
        color: suggestionToColorMap[SuggestionType.Automation] || DEFAULT_COLOR,
        description: "Send weekly language learning tips for Spanish.",
        link: "/automations?subject=Spanish Learning Tips&query=Provide a useful Spanish language learning tip, including vocabulary, grammar, or cultural insight.&crontime=0 19 * * 2",
    },
    {
        type: SuggestionType.Paint,
        color: suggestionToColorMap[SuggestionType.Paint] || DEFAULT_COLOR,
        description: "Paint a scene from a fairy tale in the style of Studio Ghibli.",
        link: "",
    },
    {
        type: SuggestionType.PopCulture,
        color: "yellow",
        description: "Find the best-rated science fiction books of the last decade.",
        link: "",
    },
    {
        type: SuggestionType.Paint,
        color: suggestionToColorMap[SuggestionType.Paint] || DEFAULT_COLOR,
        description: "Paint a still life of exotic fruits in a neon color palette.",
        link: "",
    },
    {
        type: SuggestionType.Travel,
        color: suggestionToColorMap[SuggestionType.Travel] || DEFAULT_COLOR,
        description: "Research the most eco-friendly cities in Europe.",
        link: "",
    },
    {
        type: SuggestionType.Automation,
        color: suggestionToColorMap[SuggestionType.Automation] || DEFAULT_COLOR,
        description: "Send daily reminders for habit tracking.",
        link: "/automations?subject=Habit Tracker&query=Generate a daily reminder to track habits, including a motivational message.&crontime=0 20 * * *",
    },
    {
        type: SuggestionType.Paint,
        color: suggestionToColorMap[SuggestionType.Paint] || DEFAULT_COLOR,
        description: "Create a digital painting of a cyberpunk street market.",
        link: "",
    },
    {
        type: SuggestionType.Learning,
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        description:
            "Summarize the biography of this figure: https://en.wikipedia.org/wiki/Jean_Baptiste_Point_du_Sable",
        link: "",
    },
    {
        type: SuggestionType.Language,
        color: "blue",
        description: "Send daily Spanish phrases used in Latin America.",
        link: "/automations?subject=Daily Latin American Spanish&query=Provide a common Spanish phrase or slang term used in Latin America, its meaning, and an example of usage. Include which countries it's most common in.&crontime=0 8 * * *",
    },
    {
        type: SuggestionType.Paint,
        color: suggestionToColorMap[SuggestionType.Paint] || DEFAULT_COLOR,
        description: "Create a vibrant painting inspired by Frida Kahlo's style.",
        link: "",
    },
    {
        type: SuggestionType.Food,
        color: suggestionToColorMap[SuggestionType.Food] || DEFAULT_COLOR,
        description: "Find the best empanada recipe from Colombia.",
        link: "",
    },
    {
        type: SuggestionType.Automation,
        color: suggestionToColorMap[SuggestionType.Automation] || DEFAULT_COLOR,
        description: "Weekly update on the Brazilian startup ecosystems.",
        link: "/automations?subject=LatAm Startup News&query=Provide a summary of the most significant developments in Latin American startup ecosystems this week. Include notable funding rounds, expansions, or policy changes.&crontime=0 18 * * 5",
    },
    {
        type: SuggestionType.Paint,
        color: suggestionToColorMap[SuggestionType.Paint] || DEFAULT_COLOR,
        description:
            "Paint a colorful scene of a traditional Day of the Dead celebration in Mexico.",
        link: "",
    },
    {
        type: SuggestionType.Language,
        color: suggestionToColorMap[SuggestionType.Language] || DEFAULT_COLOR,
        description: "Daily Swahili phrase with English translation.",
        link: "/automations?subject=Daily Swahili Lesson&query=Provide a common Swahili phrase or proverb, its English translation, and a brief explanation of its cultural significance in East Africa.&crontime=0 7 * * *",
    },
    {
        type: SuggestionType.Paint,
        color: suggestionToColorMap[SuggestionType.Paint] || DEFAULT_COLOR,
        description: "Create a digital painting of the Serengeti during wildebeest migration.",
        link: "",
    },
    {
        type: SuggestionType.Learning,
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        description: "Research the top M-Pesa alternatives in East Africa.",
        link: "",
    },
    {
        type: SuggestionType.Automation,
        color: suggestionToColorMap[SuggestionType.Automation] || DEFAULT_COLOR,
        description: "Weekly update on East African tech startups and innovations.",
        link: "/automations?subject=East African Tech News&query=Summarize the most significant developments in East African tech startups and innovations this week. Include notable funding rounds, new product launches, or policy changes affecting the tech ecosystem.&crontime=0 18 * * 5",
    },
    {
        type: SuggestionType.Paint,
        color: suggestionToColorMap[SuggestionType.Paint] || DEFAULT_COLOR,
        description: "Paint a colorful scene inspired by Maasai traditional clothing and jewelry.",
        link: "",
    },
    {
        type: SuggestionType.Automation,
        color: suggestionToColorMap[SuggestionType.Automation] || DEFAULT_COLOR,
        description: "Weekly summary of EU policy changes and their impact.",
        link: "/automations?subject=EU Policy Update&query=Summarize the most significant EU policy changes or proposals from this week. Explain their potential impact on European citizens and businesses.&crontime=0 17 * * 5",
    },
    {
        type: SuggestionType.Paint,
        color: suggestionToColorMap[SuggestionType.Paint] || DEFAULT_COLOR,
        description: "Paint a digital landscape of the Northern Lights over the Norwegian fjords.",
        link: "",
    },
    {
        type: SuggestionType.Automation,
        color: suggestionToColorMap[SuggestionType.Automation] || DEFAULT_COLOR,
        description: "Daily East Asian proverb with explanation.",
        link: "/automations?subject=East Asian Wisdom&query=Provide a proverb from an East Asian language (rotating through Chinese, Japanese, Korean, etc.), its English translation, and a brief explanation of its cultural significance and practical application.&crontime=0 7 * * *",
    },
    {
        type: SuggestionType.Paint,
        color: suggestionToColorMap[SuggestionType.Paint] || DEFAULT_COLOR,
        description:
            "Create a digital painting in the style of traditional Chinese ink wash landscape.",
        link: "",
    },
    {
        type: SuggestionType.PopCulture,
        color: "yellow",
        description: "Research the latest trends in K-pop and its global influence.",
        link: "",
    },
    {
        type: SuggestionType.Automation,
        color: suggestionToColorMap[SuggestionType.Automation] || DEFAULT_COLOR,
        description: "Weekly summary of technological innovations from East Asian tech giants.",
        link: "/automations?subject=East Asian Tech Update&query=Summarize the most significant technological innovations or product launches from major East Asian tech companies (e.g., Samsung, Sony, Alibaba, Tencent) this week. Explain their potential impact on global markets.&crontime=0 18 * * 5",
    },
    {
        type: SuggestionType.Paint,
        color: suggestionToColorMap[SuggestionType.Paint] || DEFAULT_COLOR,
        description: "Paint a vibrant scene of a Japanese cherry blossom festival.",
        link: "",
    },
    {
        type: SuggestionType.Automation,
        color: suggestionToColorMap[SuggestionType.Automation] || DEFAULT_COLOR,
        description: "Daily South Asian recipe with cultural significance.",
        link: "/automations?subject=South Asian Culinary Journey&query=Provide a traditional South Asian recipe (rotating through Indian, Pakistani, Bangladeshi, Sri Lankan, etc. cuisines), including ingredients, brief instructions, and its cultural significance or origin story.&crontime=0 10 * * *",
    },
    {
        type: SuggestionType.PopCulture,
        color: suggestionToColorMap[SuggestionType.PopCulture] || DEFAULT_COLOR,
        description: "Research the impact of Bollywood on global cinema and fashion.",
        link: "",
    },
    {
        type: SuggestionType.Automation,
        color: suggestionToColorMap[SuggestionType.Automation] || DEFAULT_COLOR,
        description: "Weekly update on South Asian startup ecosystems and innovations.",
        link: "/automations?subject=South Asian Startup Pulse&query=Summarize the most significant developments in South Asian startup ecosystems this week. Include notable funding rounds, innovative solutions to local challenges, and any policy changes affecting the tech landscape in countries like India, Bangladesh, Pakistan, and Sri Lanka.&crontime=0 18 * * 5",
    },
    {
        type: SuggestionType.Interviewing,
        color: suggestionToColorMap[SuggestionType.Interviewing] || DEFAULT_COLOR,
        description: "Create interview prep questions for a consulting job.",
        link: "",
    },
    {
        type: SuggestionType.Interviewing,
        color: suggestionToColorMap[SuggestionType.Interviewing] || DEFAULT_COLOR,
        description: "What information should I include in a CV for a PhD application?",
        link: "",
    },
    {
        type: SuggestionType.Home,
        color: suggestionToColorMap[SuggestionType.Home] || DEFAULT_COLOR,
        description: "Recommend plants that can grow well indoors.",
        link: "",
    },
    {
        type: SuggestionType.Health,
        color: suggestionToColorMap[SuggestionType.Health] || DEFAULT_COLOR,
        description: "Suggest healthy meal prep ideas for a busy work week.",
        link: "",
    },
    {
        type: SuggestionType.Learning,
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        description: "List effective time management techniques for students.",
        link: "",
    },
    {
        type: SuggestionType.Learning,
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        description: "Provide tips for improving public speaking skills.",
        link: "",
    },
    {
        type: SuggestionType.Learning,
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        description: "Recommend books for learning about personal finance.",
        link: "",
    },
    {
        type: SuggestionType.Home,
        color: suggestionToColorMap[SuggestionType.Home] || DEFAULT_COLOR,
        description: "Suggest ways to reduce plastic waste in daily life.",
        link: "",
    },
    {
        type: SuggestionType.Health,
        color: suggestionToColorMap[SuggestionType.Health] || DEFAULT_COLOR,
        description: "Create a beginner's guide to meditation and mindfulness.",
        link: "",
    },
    {
        type: SuggestionType.Health,
        color: suggestionToColorMap[SuggestionType.Health] || DEFAULT_COLOR,
        description: "Give me some tips for improving my sleep quality.",
        link: "",
    },
    {
        type: SuggestionType.Health,
        color: suggestionToColorMap[SuggestionType.Health] || DEFAULT_COLOR,
        description: "What are weight loss strategies supported by clinical studies?",
        link: "",
    },
    {
        type: SuggestionType.Fun,
        color: suggestionToColorMap[SuggestionType.Fun] || DEFAULT_COLOR,
        description: "List creative date ideas for couples on a budget.",
        link: "",
    },
    {
        type: SuggestionType.Code,
        color: suggestionToColorMap[SuggestionType.Interviewing] || DEFAULT_COLOR,
        description: "Provide tips for writing an effective resume.",
        link: "",
    },
    {
        type: SuggestionType.Code,
        color: suggestionToColorMap[SuggestionType.Code] || DEFAULT_COLOR,
        description: "Explain the concept of recursion with a simple coding example.",
        link: "",
    },
    {
        type: SuggestionType.Code,
        color: suggestionToColorMap[SuggestionType.Code] || DEFAULT_COLOR,
        description:
            "Provide a coding challenge to reverse a string without using built-in functions.",
        link: "",
    },
    {
        type: SuggestionType.Code,
        color: suggestionToColorMap[SuggestionType.Code] || DEFAULT_COLOR,
        description: "Explain the difference between 'let', 'const', and 'var' in JavaScript.",
        link: "",
    },
    {
        type: SuggestionType.Code,
        color: suggestionToColorMap[SuggestionType.Code] || DEFAULT_COLOR,
        description:
            "Create a coding exercise to implement a basic sorting algorithm (e.g., bubble sort).",
        link: "",
    },
    {
        type: SuggestionType.Code,
        color: suggestionToColorMap[SuggestionType.Code] || DEFAULT_COLOR,
        description: "Explain object-oriented programming principles with a simple class example.",
        link: "",
    },
    {
        type: SuggestionType.Code,
        color: suggestionToColorMap[SuggestionType.Code] || DEFAULT_COLOR,
        description:
            "Provide a coding challenge to find the longest palindromic substring in a given string.",
        link: "",
    },
    {
        type: SuggestionType.Code,
        color: suggestionToColorMap[SuggestionType.Code] || DEFAULT_COLOR,
        description:
            "Explain the concept of asynchronous programming with a JavaScript Promise example.",
        link: "",
    },
    {
        type: SuggestionType.Code,
        color: suggestionToColorMap[SuggestionType.Code] || DEFAULT_COLOR,
        description:
            "Create a coding exercise to implement a basic data structure (e.g., linked list or stack).",
        link: "",
    },
    {
        type: SuggestionType.Code,
        color: suggestionToColorMap[SuggestionType.Code] || DEFAULT_COLOR,
        description:
            "Explain the time and space complexity of common algorithms (e.g., binary search).",
        link: "",
    },
    {
        type: SuggestionType.Code,
        color: suggestionToColorMap[SuggestionType.Code] || DEFAULT_COLOR,
        description:
            "Provide a coding challenge to implement a simple REST API using Node.js and Express.",
        link: "",
    },
    {
        type: SuggestionType.Code,
        color: suggestionToColorMap[SuggestionType.Code] || DEFAULT_COLOR,
        description: "Compare popular web frameworks in Rust and Python",
        link: "",
    },
    {
        type: SuggestionType.Travel,
        color: suggestionToColorMap[SuggestionType.Travel] || DEFAULT_COLOR,
        description: "Craft an off-beat itinerary for a weekend in Lagos, Nigeria.",
        link: "",
    },
    {
        type: SuggestionType.Language,
        color: suggestionToColorMap[SuggestionType.Language] || DEFAULT_COLOR,
        description: "Teach me about declensions in Latin.",
        link: "",
    },
    {
        type: SuggestionType.Learning,
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        description: "Break down the concept of photosynthesis for a middle school student.",
        link: "",
    },
    {
        type: SuggestionType.Learning,
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        description: "Use the Socratic method to explore the causes of World War I.",
        link: "",
    },
    {
        type: SuggestionType.Learning,
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        description: "Explain the water cycle using an analogy suitable for elementary students.",
        link: "",
    },
    {
        type: SuggestionType.Learning,
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        description: "Guide a high school student through solving a quadratic equation step-by-step.",
        link: "",
    },
    {
        type: SuggestionType.Learning,
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        description: "Create a series of questions to help a student discover the principles of basic economics.",
        link: "",
    },
    {
        type: SuggestionType.Learning,
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        description: "Develop a hands-on experiment to demonstrate the concept of density to middle schoolers.",
        link: "",
    },
    {
        type: SuggestionType.Learning,
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        description: "Use guided discovery to help a student understand the structure of DNA.",
        link: "",
    },
    {
        type: SuggestionType.Learning,
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        description: "Create a personalized learning plan for a student struggling with grammar concepts.",
        link: "",
    },
    {
        type: SuggestionType.Learning,
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        description: "Design a series of questions to encourage critical thinking about climate change.",
        link: "",
    },
    {
        type: SuggestionType.Learning,
        color: suggestionToColorMap[SuggestionType.Learning] || DEFAULT_COLOR,
        description: "Develop a step-by-step guide for conducting a basic science experiment on plant growth.",
        link: "",
    },
    {
        type: SuggestionType.Health,
        color: suggestionToColorMap[SuggestionType.Health] || DEFAULT_COLOR,
        description: "Provide a detailed explanation about how to manage type 2 diabetes.",
        link: "",
    },
    {
        type: SuggestionType.Health,
        color: suggestionToColorMap[SuggestionType.Health] || DEFAULT_COLOR,
        description: "Explain the effects a stroke might have on the body.",
        link: "",
    },
    {
        type: SuggestionType.Health,
        color: suggestionToColorMap[SuggestionType.Health] || DEFAULT_COLOR,
        description: "Describe the recommended steps for preventing heart disease.",
        link: "",
    },
    {
        type: SuggestionType.Health,
        color: suggestionToColorMap[SuggestionType.Health] || DEFAULT_COLOR,
        description: "Explain the differences between various types of headaches and their treatments.",
        link: "",
    },
    {
        type: SuggestionType.Health,
        color: suggestionToColorMap[SuggestionType.Health] || DEFAULT_COLOR,
        description: "Provide an overview of the most effective stress management techniques.",
        link: "",
    },
    {
        type: SuggestionType.Health,
        color: suggestionToColorMap[SuggestionType.Health] || DEFAULT_COLOR,
        description: "Explain the importance of vaccination and how vaccines work.",
        link: "",
    },
    {
        type: SuggestionType.Health,
        color: suggestionToColorMap[SuggestionType.Health] || DEFAULT_COLOR,
        description: "Describe the symptoms and treatment options for depression.",
        link: "",
    },
    {
        type: SuggestionType.Health,
        color: suggestionToColorMap[SuggestionType.Health] || DEFAULT_COLOR,
        description: "Explain the process of digestion and common digestive disorders.",
        link: "",
    },
    {
        type: SuggestionType.Health,
        color: suggestionToColorMap[SuggestionType.Health] || DEFAULT_COLOR,
        description: "Provide an overview of the different types of cancer screenings and their importance.",
        link: "",
    },
    {
        type: SuggestionType.Health,
        color: suggestionToColorMap[SuggestionType.Health] || DEFAULT_COLOR,
        description: "Explain the effects of sleep deprivation on physical and mental health.",
        link: "",
    },
    {
        type: SuggestionType.Fun,
        color: suggestionToColorMap[SuggestionType.Fun] || DEFAULT_COLOR,
        description: "Create a list of fun activities for a family game night.",
        link: "",
    }
];
