export interface Suggestion {
    type: string;
    color: string;
    description: string;
    link: string;
}

//samples for suggestion cards (should be moved to json later)

export const suggestionsData: Suggestion[] = [
    {
        type: "Automation",
        color: "blue",
        description: "Send me a summary of HackerNews every morning.",
        link: "/automations?subject=Summarizing%20Top%20Headlines%20from%20HackerNews&query=Summarize%20the%20top%20headlines%20on%20HackerNews&crontime=00%207%20*%20*%20*",
    },
    {
        type: "Automation",
        color: "blue",
        description: "Compose a bedtime story that a five-year-old might enjoy.",
        link: "/automations?subject=Daily%20Bedtime%20Story&query=Compose%20a%20bedtime%20story%20that%20a%20five-year-old%20might%20enjoy.%20It%20should%20not%20exceed%20five%20paragraphs.%20Appeal%20to%20the%20imagination%2C%20but%20weave%20in%20learnings.&crontime=0%2021%20*%20*%20*",
    },
    {
        type: "Paint",
        color: "green",
        description: "Paint a picture of a sunset but it's made of stained glass tiles",
        link: "",
    },
    {
        type: "Online Search",
        color: "yellow",
        description: "Search for the best attractions in Austria Hungary",
        link: "",
    },
    {
        type: "Automation",
        color: "blue",
        description: "Generate a weekly meal plan with recipes.",
        link: "/automations?subject=Weekly Meal Plan&query=Create a weekly meal plan with 7 dinner recipes, including ingredients and brief instructions. Focus on balanced, healthy meals.&crontime=0 18 * * 0",
    },
    {
        type: "Paint",
        color: "green",
        description: "Paint a futuristic cityscape with flying cars.",
        link: "",
    },
    {
        type: "Online Search",
        color: "yellow",
        description: "Find the top-rated coffee shops in Seattle.",
        link: "",
    },
    {
        type: "Automation",
        color: "blue",
        description: "Send daily motivational quotes.",
        link: "/automations?subject=Daily Motivation&query=Provide an inspiring quote for the day along with a brief explanation of its meaning.&crontime=0 7 * * *",
    },
    {
        type: "Paint",
        color: "green",
        description: "Create an abstract representation of jazz music.",
        link: "",
    },
    {
        type: "Online Search",
        color: "yellow",
        description: "Research the history of the Eiffel Tower.",
        link: "",
    },
    {
        type: "Automation",
        color: "blue",
        description: "Compile a weekly news summary.",
        link: "/automations?subject=Weekly News Digest&query=Summarize the top 5 most important news stories of the week across various categories.&crontime=0 18 * * 5",
    },
    {
        type: "Paint",
        color: "green",
        description: "Paint a portrait of a cat wearing a Victorian-era costume.",
        link: "",
    },
    {
        type: "Online Search",
        color: "yellow",
        description: "Find beginner-friendly hiking trails near Los Angeles.",
        link: "",
    },
    {
        type: "Automation",
        color: "blue",
        description: "Generate a daily writing prompt.",
        link: "/automations?subject=Daily Writing Prompt&query=Create an engaging writing prompt suitable for short story or journal writing.&crontime=0 9 * * *",
    },
    {
        type: "Paint",
        color: "green",
        description: "Create a surrealist landscape inspired by Salvador Dali.",
        link: "",
    },
    {
        type: "Online Search",
        color: "yellow",
        description: "Research the benefits and drawbacks of electric vehicles.",
        link: "",
    },
    {
        type: "Automation",
        color: "blue",
        description: "Send weekly language learning tips for Spanish.",
        link: "/automations?subject=Spanish Learning Tips&query=Provide a useful Spanish language learning tip, including vocabulary, grammar, or cultural insight.&crontime=0 19 * * 2",
    },
    {
        type: "Paint",
        color: "green",
        description: "Paint a scene from a fairy tale in the style of Studio Ghibli.",
        link: "",
    },
    {
        type: "Online Search",
        color: "yellow",
        description: "Find the best-rated science fiction books of the last decade.",
        link: "",
    },
    {
        type: "Paint",
        color: "green",
        description: "Paint a still life of exotic fruits in a neon color palette.",
        link: "",
    },
    {
        type: "Online Search",
        color: "yellow",
        description: "Research the most eco-friendly cities in Europe.",
        link: "",
    },
    {
        type: "Automation",
        color: "blue",
        description: "Send daily reminders for habit tracking.",
        link: "/automations?subject=Habit Tracker&query=Generate a daily reminder to track habits, including a motivational message.&crontime=0 20 * * *",
    },
    {
        type: "Paint",
        color: "green",
        description: "Create a digital painting of a cyberpunk street market.",
        link: "",
    },
    {
        type: "Online Search",
        color: "yellow",
        description: "Summarize the biography of this figure: https://en.wikipedia.org/wiki/Jean_Baptiste_Point_du_Sable",
        link: "",
    },
    {
        type: "Automation",
        color: "blue",
        description: "Send daily Spanish phrases used in Latin America.",
        link: "/automations?subject=Daily Latin American Spanish&query=Provide a common Spanish phrase or slang term used in Latin America, its meaning, and an example of usage. Include which countries it's most common in.&crontime=0 8 * * *",
    },
    {
        type: "Paint",
        color: "green",
        description: "Create a vibrant painting inspired by Frida Kahlo's style.",
        link: "",
    },
    {
        type: "Online Search",
        color: "yellow",
        description: "Find the best empanada recipee from Colombia countries.",
        link: "",
    },
    {
        type: "Automation",
        color: "blue",
        description: "Weekly update on the Brazilian startup ecosystems.",
        link: "/automations?subject=LatAm Startup News&query=Provide a summary of the most significant developments in Latin American startup ecosystems this week. Include notable funding rounds, expansions, or policy changes.&crontime=0 18 * * 5",
    },
    {
        type: "Paint",
        color: "green",
        description: "Paint a colorful scene of a traditional Day of the Dead celebration in Mexico.",
        link: "",
    },
    {
        type: "Automation",
        color: "blue",
        description: "Daily Swahili phrase with English translation.",
        link: "/automations?subject=Daily Swahili Lesson&query=Provide a common Swahili phrase or proverb, its English translation, and a brief explanation of its cultural significance in East Africa.&crontime=0 7 * * *",
    },
    {
        type: "Paint",
        color: "green",
        description: "Create a digital painting of the Serengeti during wildebeest migration.",
        link: "",
    },
    {
        type: "Online Search",
        color: "yellow",
        description: "Research the top M-Pesa alternatives in East Africa.",
        link: "",
    },
    {
        type: "Automation",
        color: "blue",
        description: "Weekly update on East African tech startups and innovations.",
        link: "/automations?subject=East African Tech News&query=Summarize the most significant developments in East African tech startups and innovations this week. Include notable funding rounds, new product launches, or policy changes affecting the tech ecosystem.&crontime=0 18 * * 5",
    },
    {
        type: "Paint",
        color: "green",
        description: "Paint a colorful scene inspired by Maasai traditional clothing and jewelry.",
        link: "",
    },
    {
        type: "Automation",
        color: "blue",
        description: "Weekly summary of EU policy changes and their impact.",
        link: "/automations?subject=EU Policy Update&query=Summarize the most significant EU policy changes or proposals from this week. Explain their potential impact on European citizens and businesses.&crontime=0 17 * * 5",
    },
    {
        type: "Paint",
        color: "green",
        description: "Paint a digital landscape of the Northern Lights over the Norwegian fjords.",
        link: "",
    },
    {
        type: "Automation",
        color: "blue",
        description: "Daily East Asian proverb with explanation.",
        link: "/automations?subject=East Asian Wisdom&query=Provide a proverb from an East Asian language (rotating through Chinese, Japanese, Korean, etc.), its English translation, and a brief explanation of its cultural significance and practical application.&crontime=0 7 * * *",
    },
    {
        type: "Paint",
        color: "green",
        description: "Create a digital painting in the style of traditional Chinese ink wash landscape.",
        link: "",
    },
    {
        type: "Online Search",
        color: "yellow",
        description: "Research the latest trends in K-pop and its global influence.",
        link: "",
    },
    {
        type: "Automation",
        color: "blue",
        description: "Weekly summary of technological innovations from East Asian tech giants.",
        link: "/automations?subject=East Asian Tech Update&query=Summarize the most significant technological innovations or product launches from major East Asian tech companies (e.g., Samsung, Sony, Alibaba, Tencent) this week. Explain their potential impact on global markets.&crontime=0 18 * * 5",
    },
    {
        type: "Paint",
        color: "green",
        description: "Paint a vibrant scene of a Japanese cherry blossom festival.",
        link: "",
    },
    {
        type: "Automation",
        color: "blue",
        description: "Daily South Asian recipe with cultural significance.",
        link: "/automations?subject=South Asian Culinary Journey&query=Provide a traditional South Asian recipe (rotating through Indian, Pakistani, Bangladeshi, Sri Lankan, etc. cuisines), including ingredients, brief instructions, and its cultural significance or origin story.&crontime=0 10 * * *",
    },
    {
        type: "Online Search",
        color: "yellow",
        description: "Research the impact of Bollywood on global cinema and fashion.",
        link: "",
    },
    {
        type: "Automation",
        color: "blue",
        description: "Weekly update on South Asian startup ecosystems and innovations.",
        link: "/automations?subject=South Asian Startup Pulse&query=Summarize the most significant developments in South Asian startup ecosystems this week. Include notable funding rounds, innovative solutions to local challenges, and any policy changes affecting the tech landscape in countries like India, Bangladesh, Pakistan, and Sri Lanka.&crontime=0 18 * * 5",
    },
];
