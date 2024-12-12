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
1. Create a new [Text to Image Model](http://localhost:42110/server/admin/database/texttoimagemodelconfig/). Set the `type` to `Replicate`. Use any of the model names you see [on this list](https://replicate.com/pricing#image-models).

### OpenAI

1. Get [an OpenAI API key](https://platform.openai.com/settings/organization/api-keys).
2. Setup your OpenAI API key, if you haven't already. See instructions [here](/get-started/setup#2-configure)
3. Create a text to image config at http://localhost:42110/server/admin/database/texttoimagemodelconfig/. We recommend the `model name` `dall-e-3`. Make sure to associate it with the OpenAI API chat configuration you setup in step 2 with `Openai config` field.
