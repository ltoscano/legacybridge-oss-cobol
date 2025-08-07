"""Dynamic instructor mode configuration utility."""

import instructor
from typing import Optional, Any


def get_instructor_mode(instructor_mode_str: Optional[str]) -> Optional[Any]:
    """
    Get the instructor mode dynamically based on INSTRUCTOR_MODE string.
    
    Args:
        instructor_mode_str: String like "instructor.Mode.JSON" or None
        
    Returns:
        The instructor mode or None if not configured or on error.
    """
    if instructor_mode_str is None:
        return None
    
    try:
        # Parse the environment variable dynamically
        parts = instructor_mode_str.split('.')
        if not parts:
            raise ValueError("Empty or malformed INSTRUCTOR_MODE string")
        
        # Import the top-level module
        module = __import__(parts[0])
        
        # Navigate through the attributes
        for part in parts[1:]:
            module = getattr(module, part)
        
        print(f"Instructor mode set dynamically: {module}")
        return module
    
    except (ImportError, AttributeError, ValueError, IndexError) as e:
        # Handle errors: fallback to default and log
        print(f"Error parsing INSTRUCTOR_MODE '{instructor_mode_str}': {e}. Using default (instructor.Mode.JSON)")
        return instructor.Mode.JSON


def create_instructor_client(openai_client, instructor_mode_str: Optional[str] = None) -> Any:
    """
    Create instructor client with dynamic mode configuration.
    
    Args:
        openai_client: The OpenAI client instance (sync or async)
        instructor_mode_str: Optional instructor mode string override
        
    Returns:
        Configured instructor client
    """
    dynamic_mode = get_instructor_mode(instructor_mode_str)
    
    if dynamic_mode is not None:
        return instructor.from_openai(openai_client, mode=dynamic_mode)
    else:
        return instructor.from_openai(openai_client)