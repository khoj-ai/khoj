# Image Generation
You can use Khoj to generate images from text prompts. You can get deeper into the  details of our image generation flow in this blog post: https://blog.khoj.dev/posts/how-khoj-generates-images/.

To generate images, you just need to provide a prompt to Khoj in which the image generation is in the instructions. Khoj will automatically detect the image generation intent, augment your generation prompt, and then create the image. Here are some examples:
| Prompt | Image |
| --- | --- |
| Paint a picture of the plants I got last month, pixar-animation | ![plants](/img/plants_i_got.png) |
| Create a picture of my dream house, based on my interests | ![house](/img/dream_house.png) |


## Setup (Self-Hosting)

You have a couple of image generation options.

### Image Generation Models

We support most state of the art image generation models, including Ideogram, Flux, and Stable Diffusion. These will run using [Replicate](https://replicate.com). Here's how to set them up:

1. Get a Replicate API key [here](https://replicate.com/account/api-tokens).
2. Create a new [Text to Image Model](http://localhost:42110/server/admin/database/texttoimagemodelconfig/). Set the `type` to `Replicate`. Use any of the model names you see [on this list](https://replicate.com/pricing#image-models). We recommend the `model name` `black-forest-labs/flux-1.1-pro` from [Replicate](https://replicate.com/black-forest-labs/flux-1.1-pro).

### OpenAI

1. Get [an OpenAI API key](https://platform.openai.com/settings/organization/api-keys).
2. Setup your OpenAI API key, if you haven't already. See instructions [here](/get-started/setup#add-chat-models)
3. Create a text to image config at http://localhost:42110/server/admin/database/texttoimagemodelconfig/. Use `model name` `dall-e-3` to use openai for image generation. Make sure to set the `Ai model api` field to the OpenAI AI model api you setup in step 2.

### AI/ML API

1. Get [an AI/ML API key](https://aimlapi.com/app/?utm_source=khoj&utm_medium=github&utm_campaign=integration).
2. Setup your AI/ML API key, if you haven’t already. See instructions [here](https://aimlapi.com/app/keys).
3. Create a text to image config at `http://localhost:42110/server/admin/database/texttoimagemodelconfig/`.

      * **Model name**: choose one of the following IDs from our catalog:

     ```
     dall-e-3
     dall-e-2
     openai/gpt-image-1
     imagen-3.0-generate-002
     flux/schnell
     flux-pro
     flux-pro/v1.1
     flux-pro/v1.1-ultra
     flux/dev
     flux/dev/image-to-image
     stable-diffusion-v3-medium
     stable-diffusion-v35-large
     ```
   * **AI Model API**: select the “AI/ML API” entry you created in step 2.
   * Leave all other fields at their defaults.

Now your Khoj instance will route any image-generation prompt through AI/ML API’s high-quality models—just reference the model ID above when configuring.
