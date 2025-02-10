# -*- coding: utf-8 -*-
"""Langchain Agent with Mistral7B.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1e5gJaUtGVvzJP_Nr-sBEL4J2JubC5YrE
"""

!pip install langchain
!pip install accelerate
!pip install bitsandbytes

"""NOTE: You might need to restart the session after the pip install block"""

import warnings
import torch

from transformers import AutoModelForCausalLM, AutoTokenizer, BitsAndBytesConfig
from transformers.models.mistral.modeling_mistral import MistralForCausalLM
from transformers.models.llama.tokenization_llama_fast import LlamaTokenizerFast

warnings.filterwarnings('ignore')

model_name = "mistralai/Mistral-7B-Instruct-v0.2"

quantization_config = BitsAndBytesConfig(load_in_4bit=True)
model = AutoModelForCausalLM.from_pretrained(model_name, torch_dtype=torch.bfloat16, quantization_config=quantization_config, device_map="auto")
tokenizer = AutoTokenizer.from_pretrained(model_name)

from langchain.llms.base import LLM
from langchain.callbacks.manager import CallbackManagerForLLMRun
from typing import Optional, List, Mapping, Any

class CustomLLMMistral(LLM):
    model: MistralForCausalLM
    tokenizer: LlamaTokenizerFast

    @property
    def _llm_type(self) -> str:
        return "custom"

    def _call(self, prompt: str, stop: Optional[List[str]] = None,
        run_manager: Optional[CallbackManagerForLLMRun] = None) -> str:

        messages = [
         {"role": "user", "content": prompt},
        ]

        encodeds = self.tokenizer.apply_chat_template(messages, return_tensors="pt")
        model_inputs = encodeds.to(self.model.device)

        generated_ids = self.model.generate(model_inputs, max_new_tokens=512, do_sample=True, pad_token_id=tokenizer.eos_token_id, top_k=4, temperature=0.7)
        decoded = self.tokenizer.batch_decode(generated_ids)

        output = decoded[0].split("[/INST]")[1].replace("</s>", "").strip()

        if stop is not None:
          for word in stop:
            output = output.split(word)[0].strip()

        # Mistral 7B sometimes fails to properly close the Markdown Snippets.
        # If they are not correctly closed, Langchain will struggle to parse the output.
        while not output.endswith("```"):
          output += "`"

        return output

    @property
    def _identifying_params(self) -> Mapping[str, Any]:
        return {"model": self.model}

llm = CustomLLMMistral(model=model, tokenizer=tokenizer)

!pip install wikipedia

"""### Tools"""

import numexpr as ne
from langchain.tools import WikipediaQueryRun, BaseTool
from langchain_community.utilities import WikipediaAPIWrapper
from langchain.agents import Tool

"""We limit the number of results to 1 and the maximum number of characters to 2500. This limitation is imposed because, although Mistral 7B can support prompts of up to 32000 tokens, using a free Colab account would not provide sufficient memory for overly large inputs. But feel free to experiment changing the parameters."""

wikipedia = WikipediaQueryRun(api_wrapper=WikipediaAPIWrapper(top_k_results=1, doc_content_chars_max=2500))



print(wikipedia.run("Deep Learning"))

wikipedia_tool = Tool(
    name="wikipedia",
    description="Never search for more than one concept at a single step. If you need to compare two concepts, search for each one individually. Syntax: string with a simple concept",
    func=wikipedia.run
)

class Calculator(BaseTool):
    name = "calculator"
    description = "Use this tool for math operations. It requires numexpr syntax. Use it always you need to solve any math operation. Be sure syntax is correct."

    def _run(self, expression: str):
      try:
        return ne.evaluate(expression).item()
      except Exception:
        return "This is not a numexpr valid syntax. Try a different syntax."

    def _arun(self, radius: int):
        raise NotImplementedError("This tool does not support async")

calculator_tool = Calculator()
calculator_tool.run("2+3")

tools = [wikipedia_tool, calculator_tool]

"""### Customizing the Prompt"""

from langchain_core.prompts import ChatPromptTemplate, MessagesPlaceholder

system="""
You are designed to solve tasks. Each task requires multiple steps that are represented by a markdown code snippet of a json blob.
The json structure should contain the following keys:
thought -> your thoughts
action -> name of a tool
action_input -> parameters to send to the tool

These are the tools you can use: {tool_names}.

These are the tools descriptions:

{tools}

If you have enough information to answer the query use the tool "Final Answer". Its parameters is the solution.
If there is not enough information, keep trying.

"""

human="""
Add the word "STOP" after each markdown snippet. Example:

```json
{{"thought": "<your thoughts>",
 "action": "<tool name or Final Answer to give a final answer>",
 "action_input": "<tool parameters or the final output"}}
```
STOP

This is my query="{input}". Write only the next step needed to solve it.
Your answer should be based in the previous tools executions, even if you think you know the answer.
Remember to add STOP after each snippet.

These were the previous steps given to solve this query and the information you already gathered:
"""

prompt = ChatPromptTemplate.from_messages(
    [
        ("system", system),
        MessagesPlaceholder("chat_history", optional=True),
        ("human", human),
        MessagesPlaceholder("agent_scratchpad")
    ]
)

"""### Building the Agent"""

from langchain.agents import create_json_chat_agent, AgentExecutor
from langchain.memory import ConversationBufferMemory

agent = create_json_chat_agent(
    tools = tools,
    llm = llm,
    prompt = prompt,
    stop_sequence = ["STOP"],
    template_tool_response = "{observation}"
)

memory = ConversationBufferMemory(memory_key="chat_history", return_messages=True)

#agent_executor = AgentExecutor(agent=agent, tools=tools, verbose=True, handle_parsing_errors=True, memory=memory)
agent_executor = AgentExecutor(agent=agent, tools=tools, verbose=True, handle_parsing_errors=True)

"""### Tests"""

agent_executor.invoke({"input": "How much is 23 plus 17?"})

agent_executor.invoke({"input": "What is the capital of France?"})

agent_executor.invoke({"input": "Who was the inventor of the Radio?"})

agent_executor.invoke({"input": "What is the double of the population of Madrid?"})

"""The following example usually fails due to limits in the model's reasoning capacity"""

agent_executor.invoke({"input": "Who is older, Tom Hanks or Kevin Costner?"})