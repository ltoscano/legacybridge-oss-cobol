import os
import instructor
from openai import AzureOpenAI
from pydantic import BaseModel

openai_client = AzureOpenAI(
    api_key="",
    api_version="2025-01-01-preview",
    azure_endpoint="https://[].cognitiveservices.azure.com/",
)
client = instructor.from_openai(openai_client)


class User(BaseModel):
    name: str
    age: int


# Synchronous usage
user = client.chat.completions.create(
    model="gpt-4.1",
    messages=[{"role": "user", "content": "John is 30 years old"}],
    response_model=User,
)

print(user)
# > name='John' age=30
