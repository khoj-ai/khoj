import logging
import os
import uuid

logger = logging.getLogger(__name__)

AWS_ACCESS_KEY = os.getenv("AWS_ACCESS_KEY")
AWS_SECRET_KEY = os.getenv("AWS_SECRET_KEY")
# S3 supports serving assets via your domain. Khoj expects this to be used in production. To enable it:
# 1. Your bucket name for images should be of the form sub.domain.tld. For example, generated.khoj.dev
# 2. Add CNAME entry to your domain's DNS records pointing to the S3 bucket. For example, CNAME generated.khoj.dev generated-khoj-dev.s3.amazonaws.com
AWS_KHOJ_IMAGES_BUCKET_NAME = os.getenv("AWS_IMAGE_UPLOAD_BUCKET")
AWS_USER_IMAGES_BUCKET_NAME = os.getenv("AWS_USER_UPLOADED_IMAGES_BUCKET_NAME")

aws_enabled = AWS_ACCESS_KEY is not None and AWS_SECRET_KEY is not None

if aws_enabled:
    from boto3 import client

    s3_client = client("s3", aws_access_key_id=AWS_ACCESS_KEY, aws_secret_access_key=AWS_SECRET_KEY)


def upload_image_to_bucket(webp_image: bytes, user_id: uuid.UUID, bucket_name: str):
    """Upload webp image to an S3 bucket"""
    if not aws_enabled:
        logger.info("AWS is not enabled. Skipping image upload")
        return None
    if not bucket_name:
        logger.error(f"{bucket_name} is not set")
        return None

    image_key = f"{user_id}/{uuid.uuid4()}.webp"
    try:
        s3_client.put_object(
            Bucket=bucket_name,
            Key=image_key,
            Body=webp_image,
            ACL="public-read",
            ContentType="image/webp",
        )
        return f"https://{bucket_name}/{image_key}"
    except Exception as e:
        logger.error(f"Failed to upload image to S3: {e}")
        return None


def upload_generated_image_to_bucket(image: bytes, user_id: uuid.UUID):
    """Upload khoj generated image to an S3 bucket"""
    return upload_image_to_bucket(
        webp_image=image,
        user_id=user_id,
        bucket_name=AWS_KHOJ_IMAGES_BUCKET_NAME,
    )


def upload_user_image_to_bucket(image: bytes, user_id: uuid.UUID):
    """Upload user attached image to an S3 bucket"""
    return upload_image_to_bucket(
        webp_image=image,
        user_id=user_id,
        bucket_name=AWS_USER_IMAGES_BUCKET_NAME,
    )
