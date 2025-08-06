"""File management service for COBOL Migration Agents."""

import asyncio
import aiofiles
import json
from datetime import datetime
from pathlib import Path
from typing import List, Optional, Dict, Any
import logging

from ..config.settings import Settings
from ..models.cobol_models import CobolFile
from ..models.java_models import JavaFile
from ..models.dependency_models import DependencyMap


class FileManager:
    """
    Service for managing COBOL and Java file operations.
    
    Handles file discovery, reading, writing, and organization
    with support for async operations and proper error handling.
    """
    
    def __init__(self, settings: Settings):
        """Initialize the file manager."""
        self.settings = settings
        self.logger = logging.getLogger(__name__)
        
        # Supported file extensions
        self.cobol_extensions = {'.cbl', '.cpy', '.cob', '.cobol'}
        self.java_extensions = {'.java'}
    
    async def discover_cobol_files(self, source_directory: str) -> List[CobolFile]:
        """
        Discover all COBOL files in the specified directory.
        
        Args:
            source_directory: Directory to scan for COBOL files
            
        Returns:
            List of discovered COBOL files
        """
        source_path = Path(source_directory)
        if not source_path.exists():
            raise ValueError(f"Source directory does not exist: {source_directory}")
        
        cobol_files = []
        
        try:
            # Scan for COBOL files recursively
            for pattern in ['**/*.cbl', '**/*.cpy', '**/*.cob', '**/*.cobol']:
                for file_path in source_path.glob(pattern):
                    if file_path.is_file():
                        cobol_file = await self._load_cobol_file(file_path)
                        if cobol_file:
                            cobol_files.append(cobol_file)
            
            self.logger.info(f"Discovered {len(cobol_files)} COBOL files in {source_directory}")
            
        except Exception as e:
            self.logger.error(f"Error discovering COBOL files: {e}")
            raise
        
        return cobol_files
    
    async def _load_cobol_file(self, file_path: Path) -> Optional[CobolFile]:
        """
        Load a single COBOL file.
        
        Args:
            file_path: Path to the COBOL file
            
        Returns:
            CobolFile object or None if loading failed
        """
        try:
            # Read file content
            async with aiofiles.open(file_path, 'r', encoding='utf-8', errors='replace') as f:
                content = await f.read()
            
            # Get file stats
            stat = file_path.stat()
            
            # Determine file type
            file_type = self._determine_cobol_file_type(file_path.name, content)
            
            return CobolFile(
                file_name=file_path.name,
                file_path=str(file_path),
                content=content,
                file_type=file_type,
                size_bytes=stat.st_size,
                last_modified=datetime.fromtimestamp(stat.st_mtime),
                encoding='utf-8'
            )
            
        except Exception as e:
            self.logger.error(f"Failed to load COBOL file {file_path}: {e}")
            return None
    
    def _determine_cobol_file_type(self, file_name: str, content: str) -> str:
        """
        Determine the type of COBOL file based on name and content.
        
        Args:
            file_name: Name of the file
            content: File content
            
        Returns:
            File type (program, copybook, etc.)
        """
        if file_name.lower().endswith('.cpy'):
            return 'copybook'
        elif file_name.lower().endswith(('.cbl', '.cob', '.cobol')):
            # Check content for program structure
            content_upper = content.upper()
            if 'PROGRAM-ID' in content_upper:
                return 'program'
            elif 'IDENTIFICATION DIVISION' in content_upper:
                return 'program'
            else:
                return 'source'
        else:
            return 'unknown'
    
    async def save_java_file(self, java_file: JavaFile, output_directory: str) -> str:
        """
        Save a Java file to the output directory.
        
        Args:
            java_file: Java file to save
            output_directory: Directory to save the file
            
        Returns:
            Path where the file was saved
        """
        output_path = Path(output_directory)
        output_path.mkdir(parents=True, exist_ok=True)
        
        # Create package directory structure
        package_parts = java_file.package_name.split('.')
        package_dir = output_path
        for part in package_parts:
            package_dir = package_dir / part
        package_dir.mkdir(parents=True, exist_ok=True)
        
        # Write Java file
        file_path = package_dir / java_file.file_name
        
        try:
            async with aiofiles.open(file_path, 'w', encoding='utf-8') as f:
                await f.write(java_file.content)
            
            self.logger.info(f"Saved Java file: {file_path}")
            return str(file_path)
            
        except Exception as e:
            self.logger.error(f"Failed to save Java file {file_path}: {e}")
            raise
    
    async def save_dependency_map(
        self, 
        dependency_map: DependencyMap, 
        output_path: str
    ) -> str:
        """
        Save dependency map to JSON file.
        
        Args:
            dependency_map: Dependency map to save
            output_path: Path to save the JSON file
            
        Returns:
            Path where the file was saved
        """
        try:
            # Convert to dict for JSON serialization
            dependency_dict = dependency_map.model_dump()
            
            # Ensure output directory exists
            Path(output_path).parent.mkdir(parents=True, exist_ok=True)
            
            async with aiofiles.open(output_path, 'w', encoding='utf-8') as f:
                await f.write(json.dumps(dependency_dict, indent=2, default=str))
            
            self.logger.info(f"Saved dependency map: {output_path}")
            return output_path
            
        except Exception as e:
            self.logger.error(f"Failed to save dependency map {output_path}: {e}")
            raise
    
    async def save_mermaid_diagram(
        self, 
        mermaid_content: str, 
        output_path: str
    ) -> str:
        """
        Save Mermaid diagram to markdown file.
        
        Args:
            mermaid_content: Mermaid diagram content
            output_path: Path to save the markdown file
            
        Returns:
            Path where the file was saved
        """
        try:
            # Ensure output directory exists
            Path(output_path).parent.mkdir(parents=True, exist_ok=True)
            
            # Create markdown content with Mermaid diagram
            markdown_content = f"""# COBOL Dependency Diagram

Generated: {datetime.utcnow().isoformat()}

```mermaid
{mermaid_content}
```
"""
            
            async with aiofiles.open(output_path, 'w', encoding='utf-8') as f:
                await f.write(markdown_content)
            
            self.logger.info(f"Saved Mermaid diagram: {output_path}")
            return output_path
            
        except Exception as e:
            self.logger.error(f"Failed to save Mermaid diagram {output_path}: {e}")
            raise
    
    async def backup_files(self, files: List[str], backup_directory: str) -> List[str]:
        """
        Create backups of the specified files.
        
        Args:
            files: List of file paths to backup
            backup_directory: Directory to store backups
            
        Returns:
            List of backup file paths
        """
        if not self.settings.application_settings.backup_original_files:
            return []
        
        backup_path = Path(backup_directory)
        backup_path.mkdir(parents=True, exist_ok=True)
        
        backup_files = []
        timestamp = datetime.utcnow().strftime('%Y%m%d_%H%M%S')
        
        for file_path in files:
            source = Path(file_path)
            if source.exists():
                backup_name = f"{source.stem}_{timestamp}{source.suffix}"
                backup_file = backup_path / backup_name
                
                try:
                    async with aiofiles.open(source, 'rb') as src, \
                               aiofiles.open(backup_file, 'wb') as dst:
                        content = await src.read()
                        await dst.write(content)
                    
                    backup_files.append(str(backup_file))
                    self.logger.info(f"Backed up {source} to {backup_file}")
                    
                except Exception as e:
                    self.logger.error(f"Failed to backup {source}: {e}")
        
        return backup_files
    
    async def organize_output_files(self, output_directory: str) -> Dict[str, List[str]]:
        """
        Organize output files by type and create directory structure.
        
        Args:
            output_directory: Base output directory
            
        Returns:
            Dictionary mapping file types to lists of file paths
        """
        output_path = Path(output_directory)
        
        # Create standard directory structure
        directories = {
            'java': output_path / 'src' / 'main' / 'java',
            'resources': output_path / 'src' / 'main' / 'resources',
            'test': output_path / 'src' / 'test' / 'java',
            'docs': output_path / 'docs',
            'reports': output_path / 'reports',
            'logs': output_path / 'logs'
        }
        
        # Create directories
        for dir_type, dir_path in directories.items():
            dir_path.mkdir(parents=True, exist_ok=True)
        
        # Scan for existing files and organize them
        organized_files = {dir_type: [] for dir_type in directories.keys()}
        
        try:
            for file_path in output_path.rglob('*'):
                if file_path.is_file():
                    file_type = self._categorize_output_file(file_path)
                    if file_type in organized_files:
                        organized_files[file_type].append(str(file_path))
        
        except Exception as e:
            self.logger.error(f"Error organizing output files: {e}")
        
        return organized_files
    
    def _categorize_output_file(self, file_path: Path) -> str:
        """
        Categorize an output file by its type.
        
        Args:
            file_path: Path to the file
            
        Returns:
            File category
        """
        suffix = file_path.suffix.lower()
        name = file_path.name.lower()
        
        if suffix == '.java':
            return 'java'
        elif suffix in {'.md', '.txt', '.html'}:
            if 'report' in name:
                return 'reports'
            else:
                return 'docs'
        elif suffix in {'.properties', '.yml', '.yaml', '.xml'}:
            return 'resources'
        elif suffix in {'.log', '.json'} and 'log' in name:
            return 'logs'
        else:
            return 'docs'
    
    async def get_file_statistics(self, directory: str) -> Dict[str, Any]:
        """
        Get statistics about files in a directory.
        
        Args:
            directory: Directory to analyze
            
        Returns:
            Dictionary with file statistics
        """
        dir_path = Path(directory)
        if not dir_path.exists():
            return {}
        
        stats = {
            'total_files': 0,
            'total_size_bytes': 0,
            'file_types': {},
            'largest_file': None,
            'smallest_file': None,
            'newest_file': None,
            'oldest_file': None
        }
        
        try:
            files_info = []
            
            for file_path in dir_path.rglob('*'):
                if file_path.is_file():
                    stat = file_path.stat()
                    file_info = {
                        'path': str(file_path),
                        'size': stat.st_size,
                        'modified': datetime.fromtimestamp(stat.st_mtime),
                        'extension': file_path.suffix.lower()
                    }
                    files_info.append(file_info)
            
            if files_info:
                stats['total_files'] = len(files_info)
                stats['total_size_bytes'] = sum(f['size'] for f in files_info)
                
                # File type distribution
                for file_info in files_info:
                    ext = file_info['extension'] or 'no_extension'
                    stats['file_types'][ext] = stats['file_types'].get(ext, 0) + 1
                
                # Extremes
                stats['largest_file'] = max(files_info, key=lambda x: x['size'])['path']
                stats['smallest_file'] = min(files_info, key=lambda x: x['size'])['path']
                stats['newest_file'] = max(files_info, key=lambda x: x['modified'])['path']
                stats['oldest_file'] = min(files_info, key=lambda x: x['modified'])['path']
        
        except Exception as e:
            self.logger.error(f"Error getting file statistics: {e}")
        
        return stats