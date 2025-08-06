"""Tests for data models."""

import pytest
from datetime import datetime
from cobol_migration_agents.models.cobol_models import CobolFile, CobolAnalysis, CobolProgram, CobolDataItem
from cobol_migration_agents.models.java_models import JavaFile, JavaClass, JavaMethod
from cobol_migration_agents.models.dependency_models import DependencyMap, DependencyMapMetrics, DependencyRelation


def test_cobol_file_creation():
    """Test CobolFile model creation."""
    cobol_file = CobolFile(
        file_name="TEST.cbl",
        file_path="/path/to/TEST.cbl",
        content="IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.",
        file_type="program",
        size_bytes=1024
    )
    
    assert cobol_file.file_name == "TEST.cbl"
    assert cobol_file.file_type == "program"
    assert cobol_file.size_bytes == 1024
    assert cobol_file.encoding == "utf-8"


def test_cobol_analysis_creation():
    """Test CobolAnalysis model creation."""
    analysis = CobolAnalysis(
        file_name="TEST.cbl",
        analysis_date=datetime.utcnow(),
        cyclomatic_complexity=5,
        migration_complexity="medium",
        uses_goto=True,
        business_rules=["Validate customer ID", "Calculate total"]
    )
    
    assert analysis.file_name == "TEST.cbl"
    assert analysis.cyclomatic_complexity == 5
    assert analysis.migration_complexity == "medium"
    assert analysis.uses_goto is True
    assert len(analysis.business_rules) == 2


def test_cobol_data_item():
    """Test CobolDataItem model."""
    data_item = CobolDataItem(
        name="WS-CUSTOMER-ID",
        level=5,
        picture="9(10)",
        usage="COMP",
        value="SPACES"
    )
    
    assert data_item.name == "WS-CUSTOMER-ID"
    assert data_item.level == 5
    assert data_item.picture == "9(10)"
    assert data_item.usage == "COMP"


def test_java_file_creation():
    """Test JavaFile model creation."""
    java_file = JavaFile(
        file_name="Customer.java",
        package_name="com.example.model",
        class_name="Customer",
        content="package com.example.model;\n\npublic class Customer {}",
        original_cobol_file_name="CUSTOMER.cbl",
        is_entity=True,
        estimated_accuracy=0.95
    )
    
    assert java_file.file_name == "Customer.java"
    assert java_file.package_name == "com.example.model"
    assert java_file.class_name == "Customer"
    assert java_file.is_entity is True
    assert java_file.estimated_accuracy == 0.95


def test_java_method():
    """Test JavaMethod model."""
    method = JavaMethod(
        name="calculateTotal",
        return_type="BigDecimal",
        parameters=["BigDecimal amount", "BigDecimal tax"],
        body="return amount.add(tax);",
        annotations=["@Override"]
    )
    
    assert method.name == "calculateTotal"
    assert method.return_type == "BigDecimal"
    assert len(method.parameters) == 2
    assert method.annotations == ["@Override"]


def test_dependency_relation():
    """Test DependencyRelation model."""
    relation = DependencyRelation(
        source_file="MAIN.cbl",
        target_file="UTILS.cpy",
        dependency_type="COPY",
        line_number=10,
        context="COPY UTILS."
    )
    
    assert relation.source_file == "MAIN.cbl"
    assert relation.target_file == "UTILS.cpy"
    assert relation.dependency_type == "COPY"
    assert relation.line_number == 10


def test_dependency_map():
    """Test DependencyMap model."""
    metrics = DependencyMapMetrics(
        total_files=5,
        total_programs=3,
        total_copybooks=2,
        total_dependencies=10,
        average_dependencies_per_program=3.3,
        max_dependencies_for_single_program=5,
        programs_with_no_dependencies=0
    )
    
    dependency_map = DependencyMap(
        source_directory="/path/to/cobol",
        metrics=metrics,
        mermaid_diagram="graph TD\n    A --> B"
    )
    
    assert dependency_map.source_directory == "/path/to/cobol"
    assert dependency_map.metrics.total_files == 5
    assert dependency_map.metrics.average_dependencies_per_program == 3.3
    assert "graph TD" in dependency_map.mermaid_diagram


def test_dependency_map_methods():
    """Test DependencyMap utility methods."""
    dependency_map = DependencyMap(
        source_directory="/test",
        metrics=DependencyMapMetrics(
            total_files=0, total_programs=0, total_copybooks=0,
            total_dependencies=0, average_dependencies_per_program=0,
            max_dependencies_for_single_program=0, programs_with_no_dependencies=0
        ),
        program_dependencies={"MAIN.cbl": ["UTILS.cpy", "DATA.cpy"]},
        reverse_dependencies={"UTILS.cpy": ["MAIN.cbl", "OTHER.cbl"]}
    )
    
    # Test get_dependencies_for_file
    deps = dependency_map.get_dependencies_for_file("MAIN.cbl")
    assert len(deps) == 2
    assert "UTILS.cpy" in deps
    
    # Test get_dependents_of_file
    dependents = dependency_map.get_dependents_of_file("UTILS.cpy")
    assert len(dependents) == 2
    assert "MAIN.cbl" in dependents
    
    # Test is_copybook
    assert dependency_map.is_copybook("UTILS.cpy") is True
    assert dependency_map.is_copybook("MAIN.cbl") is False
    
    # Test get_migration_complexity
    complexity = dependency_map.get_migration_complexity("MAIN.cbl")
    assert complexity in ["low", "medium", "high"]


def test_model_serialization():
    """Test that models can be serialized to JSON."""
    cobol_file = CobolFile(
        file_name="TEST.cbl",
        file_path="/path/to/TEST.cbl",
        content="PROGRAM-ID. TEST.",
        file_type="program",
        size_bytes=100
    )
    
    # Test model_dump (Pydantic v2)
    data = cobol_file.model_dump()
    assert data["file_name"] == "TEST.cbl"
    assert data["file_type"] == "program"
    
    # Test model_validate (Pydantic v2)
    reconstructed = CobolFile.model_validate(data)
    assert reconstructed.file_name == cobol_file.file_name
    assert reconstructed.content == cobol_file.content


if __name__ == "__main__":
    pytest.main([__file__])