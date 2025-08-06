"""Setup script for COBOL Migration Agents."""

from setuptools import setup, find_packages
import pathlib

# Read the contents of README file
this_directory = pathlib.Path(__file__).parent
long_description = (this_directory / "README.md").read_text()

setup(
    name="cobol-migration-agents",
    version="0.1.0",
    description="AI-powered COBOL to Java Quarkus migration using atomic-agents",
    long_description=long_description,
    long_description_content_type="text/markdown",
    author="Your Name",
    author_email="your.email@example.com",
    packages=find_packages(),
    python_requires=">=3.10",
    install_requires=[
        "atomic-agents>=0.1.0",
        "instructor>=1.0.0", 
        "openai>=1.0.0",
        "pydantic>=2.0.0",
        "rich>=13.0.0",
        "typer>=0.9.0",
        "httpx>=0.25.0",
        "python-dotenv>=1.0.0",
        "aiofiles>=23.0.0",
    ],
    extras_require={
        "dev": [
            "pytest>=7.0.0",
            "pytest-asyncio>=0.21.0",
            "black>=23.0.0",
            "flake8>=6.0.0",
            "mypy>=1.0.0",
        ]
    },
    entry_points={
        "console_scripts": [
            "cobol-migrate=cobol_migration_agents.cli:app",
            "cobol-migrate-conversation=cobol_migration_agents.cli:conversation_command",
            "cobol-migrate-setup=cobol_migration_agents.cli:setup_command",
        ],
    },
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: MIT License",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
    ],
)