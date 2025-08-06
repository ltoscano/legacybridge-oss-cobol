#!/usr/bin/env python3
"""Setup script for COBOL Migration Agents."""

import os
import sys
from pathlib import Path

def main():
    """Main setup function."""
    print("🔧 COBOL Migration Agents Setup")
    print("=" * 40)
    
    # Check Python version
    if sys.version_info < (3, 10):
        print("❌ Python 3.10 or higher is required")
        print(f"Current version: {sys.version}")
        sys.exit(1)
    
    print("✅ Python version check passed")
    
    # Check if we're in a virtual environment
    if sys.prefix == sys.base_prefix:
        print("⚠️  Warning: You're not in a virtual environment")
        print("It's recommended to use a virtual environment")
        response = input("Continue anyway? (y/N): ")
        if response.lower() != 'y':
            print("Setup cancelled")
            sys.exit(0)
    
    # Check for Poetry
    poetry_available = os.system("poetry --version > /dev/null 2>&1") == 0
    
    if poetry_available:
        print("📦 Installing dependencies with Poetry...")
        os.system("poetry install")
    else:
        print("📦 Installing dependencies with pip...")
        os.system("pip install -e .")
    
    # Create config directory
    config_dir = Path("config")
    config_dir.mkdir(exist_ok=True)
    print(f"📁 Created config directory: {config_dir}")
    
    # Copy example config if local config doesn't exist
    local_config = config_dir / "settings.local.env"
    example_config = config_dir / "settings.env.example"
    
    if not local_config.exists() and example_config.exists():
        with open(example_config, 'r') as src, open(local_config, 'w') as dst:
            dst.write(src.read())
        print(f"📋 Created local config template: {local_config}")
        print("⚠️  Please edit this file with your actual API credentials")
    
    # Create logs directory
    logs_dir = Path("logs")
    logs_dir.mkdir(exist_ok=True)
    print(f"📁 Created logs directory: {logs_dir}")
    
    # Create example directories
    example_dirs = ["cobol-source", "java-output"]
    for dir_name in example_dirs:
        example_dir = Path(dir_name)
        example_dir.mkdir(exist_ok=True)
        print(f"📁 Created example directory: {example_dir}")
    
    print("\n✅ Setup completed successfully!")
    print("\nNext steps:")
    print("1. Edit config/settings.local.env with your AI service credentials")
    print("2. Run: cobol-migrate-setup (for interactive configuration)")
    print("3. Test with: cobol-migrate --cobol-source ./cobol-source --java-output ./java-output")
    print("4. Check documentation in README.md")

if __name__ == "__main__":
    main()