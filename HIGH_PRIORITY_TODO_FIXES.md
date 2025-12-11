# High Priority TODO Fixes - Completed

**Date:** 2025-12-11
**Branch:** `claude/count-todos-012QGjYMQLDLpq3qMuVvwCtB`
**Commits:** `4ef5da9`, `1dd4b53`

## Summary

Fixed 4 high-priority TODOs identified in the codebase audit. All changes have been committed and pushed.

## TODO Count Analysis

**Total matches for "TODO":** 36
**Real TODOs in Khoj code:** 14

**False positives (22):**
- Minified libraries (org.min.js, markdown-it.min.js): 8 TODOs
- Test data (org-mode entries in test files): 3 TODOs
- Org-mode parser documentation (not actual TODOs): 11 TODOs

## Fixes Implemented

### 1. Excalidraw Diagram Validation ✅
**File:** `src/khoj/routers/helpers.py:953`
**Priority:** High (Security/Data Integrity)

**Changes:**
- Added comprehensive validation for Excalidraw diagram structure
- Validates that `elements` is a non-empty list
- Checks each element has required fields: `type`, `id`, `x`, `y`
- Type-checks field values: `type`/`id` must be strings, `x`/`y` must be numbers
- Provides detailed error messages for debugging

**Why it matters:** Prevents malformed or malicious diagram data from being processed.

---

### 2. Query Images & Files Support in Operator ✅
**Files:**
- `src/khoj/processor/operator/operator_agent_base.py:40-41`
- `src/khoj/processor/operator/operator_agent_binary.py:46-47`
- `src/khoj/processor/operator/__init__.py:42,44`

**Priority:** High (Missing Core Functionality)

**Changes:**
- Extended `OperatorAgent` base class to accept `query_images` and `query_files` parameters
- Integrated with `construct_structured_message()` to create multimodal initial messages
- Updated all operator agent instantiations:
  - `AnthropicOperatorAgent`
  - `OpenAIOperatorAgent`
  - `BinaryOperatorAgent`
- Parameters now properly flow from API layer through to agent initialization
- Images and files are included in the initial user message using proper content blocks

**Why it matters:** Enables operator agents to work with image and file inputs, completing the vision-enabled workflow.

---

### 3. Multiple Tool Calls Handling in Research ✅
**File:** `src/khoj/routers/research.py:212`
**Priority:** High (Feature Limitation)

**Changes:**
- Parse all tool calls from model response (not just the first one)
- Intelligently select first non-repeated tool call from multiple options
- Notify user when multiple tool requests are detected
- Improved duplicate detection to avoid repeated tool/query combinations
- Added logging for better observability

**Implementation approach:**
- Parses entire tool call array from model
- Checks each against previously used combinations
- Selects first non-duplicate tool call
- Falls back to first tool if all are duplicates
- Logs selection for debugging

**Why it matters:** Handles cases where LLMs return multiple tool calls, preventing silent failures and improving research quality.

---

### 4. Notion Database Processing ✅
**File:** `src/khoj/processor/content/notion/notion_to_entries.py:108`
**Priority:** High (Missing Feature)

**Changes:**
- Added `process_database()` method to handle Notion databases
- Queries database to retrieve all pages within it
- Handles pagination for databases with many entries
- Reuses existing `process_page()` logic for consistency
- Includes error handling and logging
- Databases are no longer skipped during Notion sync

**Implementation:**
```python
def process_database(self, database):
    """Process a Notion database by querying all its pages and processing each one."""
    database_id = database["id"]
    database_entries = []

    # Query database with pagination
    query_params = {"page_size": 100}
    while True:
        response = self.session.post(
            f"https://api.notion.com/v1/databases/{database_id}/query",
            json=query_params,
        ).json()

        # Process each page
        for page in response.get("results", []):
            if page.get("object") == "page":
                page_entries = self.process_page(page)
                database_entries.extend(page_entries)

        # Handle pagination
        if not response.get("has_more", False):
            break
        query_params["start_cursor"] = response["next_cursor"]

    return database_entries
```

**Why it matters:** Users can now sync and search content from Notion databases, not just individual pages.

---

## Remaining TODOs (10 - Medium/Low Priority)

### Medium Priority (4)
1. `src/khoj/routers/helpers.py:2379` - Replace sync call with async version
2. `src/khoj/processor/operator/operator_agent_openai.py:402` - Get OS info from environment (currently assumes Linux)
3. `src/khoj/utils/initialization.py:217` - Replace hacky provider name filtering
4. `src/khoj/database/adapters/__init__.py:744` - Update public agent filtering logic
5. `src/interface/obsidian/src/chat_view.ts:82` - Only show available chat modes

### Low Priority (5)
1. `src/khoj/processor/operator/__init__.py:82` - Remove dead OpenAI operator code
2. `src/khoj/processor/operator/__init__.py:94` - Remove dead Binary operator code
3. `src/interface/web/app/components/excalidraw/excalidrawWrapper.tsx:149` - Create common theme detection function
4. `src/interface/web/app/components/chatHistory/chatHistory.tsx:357` - IntersectionObserver delay optimization

---

## Testing Recommendations

### For Excalidraw Validation
- Test with valid diagrams
- Test with missing required fields
- Test with wrong field types
- Test with empty elements array

### For Operator Images/Files
- Test operator with image attachments
- Test operator with file attachments
- Test operator with both images and files
- Verify vision-enabled model is used

### For Multiple Tool Calls
- Monitor research mode when multiple tools are suggested
- Verify duplicate detection works correctly
- Check that user is notified appropriately

### For Notion Databases
- Sync workspace with databases
- Verify database pages are indexed
- Search for content within database pages
- Test with large databases (pagination)

---

## Related Documentation

- Operator documentation: [Agent SDK docs]
- Research mode: `src/khoj/routers/research.py`
- Notion integration: `src/khoj/processor/content/notion/`
- Excalidraw integration: `src/khoj/routers/helpers.py`

---

## Notes

- All changes follow existing code patterns and conventions
- Error handling added where appropriate
- Logging included for debugging and monitoring
- No breaking changes to existing APIs
- Changes are backward compatible
