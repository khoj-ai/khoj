# Migration Plan: Personal Life OS with AI

## Executive Summary

This document analyzes whether [Khoj](https://github.com/khoj-ai/khoj) is the right foundation for building an **AI-powered personal life management system** — a self-hosted, open-source alternative to tools like Monday.com, ClickUp, or Notion, enhanced with deep LLM integration for preference learning, life coaching, and proactive assistance.

### The Vision
A system where users can:
- **Brain dump** life events, thoughts, and feelings in natural conversation
- Have the AI **learn preferences and patterns** over time (a "memory map")
- Create and track **tasks/goals** directly from conversations
- Receive **smart nudges** and gap analysis from the AI
- Conduct **daily/weekly check-ins** with a personal AI assistant
- All **self-hosted** and **open source**

### The Verdict

**Khoj is a strong foundation** — but it requires significant extension to achieve the full vision.

| Aspect | Khoj Provides | Needs Building |
|--------|---------------|----------------|
| LLM Integration | ✅ Excellent (OpenAI, Anthropic, Google, local) | — |
| Conversation Memory | ✅ Full history stored per user | Cross-session preference extraction |
| Knowledge Search | ✅ Semantic search with pgvector | — |
| Automation/Scheduling | ✅ Cron-based with email delivery | Action-based task execution |
| Agent System | ✅ Custom personalities and tools | Life coach persona refinement |
| Task Management | ❌ No todo/project system | Full task/goal tracking system |
| Journaling | ⚠️ Reflective questions only | Structured journaling with mood/life tracking |
| Nudge System | ❌ No proactive suggestions | Smart notification engine |
| Preference Learning | ❌ No automatic extraction | Memory map construction |

**Recommendation: Fork Khoj** and extend it with new subsystems for task management, journaling, preference learning, and proactive nudges. The core LLM infrastructure, search capabilities, and multi-platform architecture are too valuable to rebuild.

---

## Table of Contents

1. [Current State Analysis](#current-state-analysis)
2. [Gap Analysis](#gap-analysis)
3. [Execution Phases](#execution-phases)
4. [Priority Matrix](#priority-matrix)
5. [Technology Recommendations](#technology-recommendations)
6. [Research Recommendations](#research-recommendations)
7. [Fork vs Fresh Start](#fork-vs-fresh-start)
8. [Open Source Release Strategy](#open-source-release-strategy)

---

## Current State Analysis

### What Khoj Already Provides

#### 1. **Robust LLM Infrastructure** (Ready to Use)
- Multi-provider support: OpenAI, Anthropic Claude, Google Gemini, local models (Ollama)
- Streaming responses with tool calling
- Structured output support (JSON schemas)
- Extended thinking/reasoning support (O1/O3-style models)
- Token management and context window optimization

#### 2. **Conversation System** (Ready to Use)
- Full multi-turn conversation persistence in PostgreSQL
- Multiple concurrent conversation threads per user
- Conversation titles, search, and export
- Agent association per conversation
- File/context filters per conversation

#### 3. **Knowledge/Memory System** (Partially Suitable)
- Semantic search using sentence-transformers + pgvector
- Document ingestion: PDF, Markdown, Org-mode, Notion, GitHub, DOCX
- Cross-encoder reranking for relevance
- Per-user knowledge isolation
- Agent-specific knowledge bases

#### 4. **Agent Architecture** (Excellent Foundation)
- Custom agent personalities and system prompts
- Configurable tool access per agent (web search, code execution, notes)
- Public/private/protected agent privacy levels
- MCP (Model Context Protocol) for external tool integration

#### 5. **Automation System** (Needs Extension)
- Cron-based scheduling (APScheduler + Django job store)
- Query execution with email delivery
- Timezone-aware execution
- Conversation thread per automation

#### 6. **Multi-Platform Support** (Ready to Use)
- Web (Next.js)
- Desktop (Tauri)
- Mobile (Capacitor - iOS/Android)
- Integrations: Obsidian, Emacs, WhatsApp

#### 7. **Reflective Questions** (Seed for Journaling)
- `ReflectiveQuestion` model for prompts
- Random selection as conversation starters
- Global vs user-specific questions

---

## Gap Analysis

### Missing Systems Required for Vision

#### 1. **Preference Learning / Memory Map** (High Priority)
**Current State**: Conversation history exists but preferences are not extracted.

**What's Needed**:
- Automatic extraction of user preferences from conversations
- Structured preference storage (likes/dislikes, communication style, domain expertise)
- Correction detection and learning (when user says "not like that, like this")
- Preference decay/update over time
- Preference injection into all LLM system prompts

**Proposed Model**:
```python
class UserPreference(models.Model):
    user = models.ForeignKey(KhojUser)
    category = models.CharField()  # "communication_style", "design_taste", "values"
    key = models.CharField()       # "prefers_minimal_design", "dislikes_emojis"
    value = models.JSONField()     # {"preference": "minimal", "confidence": 0.85}
    source_conversation = models.ForeignKey(Conversation, null=True)
    extracted_at = models.DateTimeField()
    last_reinforced = models.DateTimeField()
    reinforcement_count = models.IntegerField(default=1)
```

#### 2. **Task/Goal Management System** (High Priority)
**Current State**: Only automation queries exist, no actionable task tracking.

**What's Needed**:
- Task creation from natural conversation ("I need to finish the proposal by Friday")
- Task states: pending, in_progress, completed, blocked, cancelled
- Priority levels and due dates
- Goal decomposition (goals → milestones → tasks)
- Project/area organization
- Recurring tasks
- Task dependencies
- Progress visualization

**Proposed Models**:
```python
class Goal(models.Model):
    user = models.ForeignKey(KhojUser)
    title = models.CharField()
    description = models.TextField()
    status = models.CharField()  # active, paused, completed, abandoned
    target_date = models.DateField(null=True)
    created_from_conversation = models.ForeignKey(Conversation, null=True)
    progress_percentage = models.IntegerField(default=0)

class Task(models.Model):
    user = models.ForeignKey(KhojUser)
    goal = models.ForeignKey(Goal, null=True)
    title = models.CharField()
    description = models.TextField()
    status = models.CharField()  # pending, in_progress, completed, blocked
    priority = models.CharField()  # low, medium, high, urgent
    due_date = models.DateTimeField(null=True)
    recurrence_rule = models.CharField(null=True)  # RRULE format
    parent_task = models.ForeignKey('self', null=True)
    created_from_conversation = models.ForeignKey(Conversation, null=True)
    blocked_by = models.ManyToManyField('self')
```

#### 3. **Journaling & Check-in System** (Medium Priority)
**Current State**: Only reflective question prompts, no journal storage.

**What's Needed**:
- Structured journal entries (not just conversations)
- Daily/weekly check-in scheduling
- Mood/energy tracking with temporal visualization
- Life area tracking (work, relationships, health, personal growth)
- AI-generated insights from journal patterns
- Private reflection space distinct from task-oriented chat

**Proposed Models**:
```python
class JournalEntry(models.Model):
    user = models.ForeignKey(KhojUser)
    entry_type = models.CharField()  # freeform, daily_checkin, weekly_review
    content = models.TextField()
    mood_score = models.IntegerField(null=True)  # 1-10
    energy_level = models.IntegerField(null=True)
    life_areas = models.JSONField()  # {"work": 7, "relationships": 8, ...}
    key_themes = models.JSONField()  # AI-extracted themes
    created_at = models.DateTimeField()
    associated_conversation = models.ForeignKey(Conversation, null=True)

class CheckInSchedule(models.Model):
    user = models.ForeignKey(KhojUser)
    check_in_type = models.CharField()  # daily, weekly, custom
    cron_expression = models.CharField()
    prompt_template = models.TextField()
    notification_method = models.CharField()  # email, push, in_app
```

#### 4. **Proactive Nudge System** (Medium Priority)
**Current State**: Only automation email delivery, no smart suggestions.

**What's Needed**:
- Pattern detection from user behavior and journal entries
- Gap analysis (identifying unfulfilled intentions)
- Smart reminder timing (not just cron-based)
- Contextual nudges based on calendar, location, mood
- Notification preferences and frequency limits
- Nudge effectiveness tracking

**Proposed Models**:
```python
class Nudge(models.Model):
    user = models.ForeignKey(KhojUser)
    nudge_type = models.CharField()  # reminder, suggestion, insight, question
    content = models.TextField()
    reasoning = models.TextField()  # Why this nudge was generated
    trigger_context = models.JSONField()
    priority = models.CharField()
    scheduled_for = models.DateTimeField()
    delivered_at = models.DateTimeField(null=True)
    user_response = models.CharField(null=True)  # acted, dismissed, snoozed
    related_goal = models.ForeignKey(Goal, null=True)
    related_task = models.ForeignKey(Task, null=True)
```

#### 5. **Life Coach Agent Persona** (Lower Priority)
**Current State**: Generic agent personalities.

**What's Needed**:
- Specialized "life coach" agent with appropriate prompting
- Understanding of behavioral change principles
- Empathetic communication patterns
- Non-judgmental reflection facilitation
- Goal-setting frameworks (SMART, OKRs, etc.)
- Accountability patterns

---

## Execution Phases

### Phase 1: Foundation & Core Models (Weeks 1-3)
**Priority: Critical**

1. **Fork and setup**
   - Fork Khoj repository
   - Set up development environment
   - Establish CI/CD pipeline
   - Create feature branch structure

2. **Add core data models**
   - `UserPreference` model and migrations
   - `Goal` and `Task` models and migrations
   - `JournalEntry` and `CheckInSchedule` models
   - `Nudge` model for future use

3. **Create API scaffolding**
   - `/api/tasks` CRUD endpoints
   - `/api/goals` CRUD endpoints
   - `/api/journal` endpoints
   - `/api/preferences` read/update endpoints

### Phase 2: Conversational Task Creation (Weeks 4-6)
**Priority: High**

1. **Task extraction from conversation**
   - Train/prompt LLM to identify actionable items in conversations
   - Create extraction pipeline (conversation → task candidates)
   - User confirmation flow for task creation
   - Link tasks back to source conversation

2. **Task management UI**
   - Task list view with filtering
   - Kanban board view
   - Task detail/edit modal
   - Goal overview with progress

3. **Integrate with existing conversation**
   - Add `/task` command to create tasks inline
   - Show relevant tasks in conversation context
   - Update task status from conversation

### Phase 3: Preference Learning (Weeks 7-9)
**Priority: High**

1. **Preference extraction pipeline**
   - Analyze conversation history for preference signals
   - Detect corrections and adjust preferences
   - Categorize preferences (aesthetic, communication, values, domain)
   - Confidence scoring and decay

2. **Memory map construction**
   - Aggregate preferences into user profile
   - Version preference history
   - Allow user review and override

3. **Preference injection**
   - Modify system prompts to include relevant preferences
   - Context-aware preference selection (design task → design preferences)
   - A/B test preference impact

### Phase 4: Journaling & Check-ins (Weeks 10-12)
**Priority: Medium**

1. **Journal entry system**
   - Dedicated journaling interface
   - Structured check-in templates
   - Mood and energy tracking
   - Life area assessment

2. **Scheduled check-ins**
   - Extend automation system for check-in reminders
   - Custom check-in prompts
   - Multi-channel delivery (email, push)

3. **Journal insights**
   - Pattern detection over time
   - Mood trends visualization
   - Theme extraction and tagging
   - Correlation analysis (mood ↔ activities)

### Phase 5: Proactive Nudges (Weeks 13-16)
**Priority: Medium**

1. **Nudge generation engine**
   - Analyze goals, tasks, and journal for nudge opportunities
   - Gap detection (intentions not acted upon)
   - Smart timing optimization
   - Contextual relevance scoring

2. **Notification infrastructure**
   - Push notification support (web, mobile)
   - In-app notification center
   - Notification preferences and limits
   - Delivery tracking

3. **Nudge effectiveness loop**
   - Track user responses to nudges
   - Adjust nudge frequency and content based on engagement
   - Learn optimal timing per user

### Phase 6: Polish & Integration (Weeks 17-20)
**Priority: Lower**

1. **Life coach agent refinement**
   - Specialized agent personality and prompts
   - Behavioral change frameworks
   - Empathetic response patterns

2. **Dashboard and visualization**
   - Life overview dashboard
   - Goal progress charts
   - Mood trends over time
   - Activity heatmaps

3. **Export and interoperability**
   - Export to standard formats (ICAL, JSON, Markdown)
   - Import from other tools
   - API for external integrations

---

## Priority Matrix

| Feature | Impact | Effort | Priority | Phase |
|---------|--------|--------|----------|-------|
| Core data models (Task, Goal, Preference) | High | Medium | P0 | 1 |
| Task creation from conversation | High | Medium | P0 | 2 |
| Preference extraction pipeline | High | High | P1 | 3 |
| Task management UI | High | Medium | P1 | 2 |
| Basic journaling | Medium | Low | P2 | 4 |
| Daily check-in scheduling | Medium | Low | P2 | 4 |
| Proactive nudge system | Medium | High | P2 | 5 |
| Mood tracking and visualization | Medium | Medium | P3 | 4 |
| Push notifications | Low | Medium | P3 | 5 |
| Life coach agent refinement | Low | Medium | P3 | 6 |
| Dashboard and analytics | Low | High | P4 | 6 |

---

## Technology Recommendations

### Leverage Existing (From Khoj)

| Component | Technology | Reason |
|-----------|------------|--------|
| Backend Framework | FastAPI + Django | Already integrated, excellent async support |
| Database | PostgreSQL + pgvector | Vector search ready, robust relational store |
| LLM Integration | Existing adapters | OpenAI, Anthropic, Google already work |
| Frontend | Next.js | Modern React, SSR, good DX |
| Mobile | Capacitor | Cross-platform from existing web app |
| Task Scheduling | APScheduler | Already used for automations |

### Add New

| Need | Recommended Technology | Alternative |
|------|----------------------|-------------|
| Push Notifications | [Novu](https://novu.co/) (open source) | Firebase Cloud Messaging, OneSignal |
| Recurring Tasks | [python-dateutil](https://dateutil.readthedocs.io/) RRULE | django-recurrence |
| Task UI Components | [React Beautiful DnD](https://github.com/atlassian/react-beautiful-dnd) | dnd-kit |
| Charts/Visualization | [Recharts](https://recharts.org/) or [Tremor](https://tremor.so/) | Chart.js |
| Notification Queue | Redis + Celery | Existing APScheduler |
| Preference Store | PostgreSQL JSONB | Redis for hot cache |

### Consider Integrating

| Tool | Purpose | Integration Approach |
|------|---------|---------------------|
| [Cal.com](https://cal.com/) | Calendar integration | API + OAuth |
| [Plane](https://plane.so/) | Project management inspiration | UI patterns |
| [Logseq](https://logseq.com/) / [Obsidian](https://obsidian.md/) | Note sync | Existing Obsidian integration |
| [ntfy](https://ntfy.sh/) | Self-hosted push notifications | Simple HTTP API |

---

## Research Recommendations

### Academic/Technical Research

1. **Preference Learning from Dialogue**
   - Paper: "Learning User Preferences from Dialogue Interactions" (ACL 2023)
   - Explore: Reinforcement Learning from Human Feedback (RLHF) techniques for preference extraction

2. **Behavioral Nudge Design**
   - Book: "Nudge" by Thaler & Sunstein
   - Paper: "Digital Nudging: A Review of Design Approaches for Self-Tracking Technologies"

3. **Life Logging and Quantified Self**
   - Explore: [Quantified Self](https://quantifiedself.com/) community projects
   - Paper: "Personal Informatics and Context" (CHI 2023)

4. **AI Coaching Effectiveness**
   - Paper: "Conversational Agents for Mental Health" (Nature Digital Medicine 2022)
   - Note ethical considerations around AI life coaching

### Open Source Projects to Study

| Project | What to Learn |
|---------|---------------|
| [Plane](https://github.com/makeplane/plane) | Modern project management UI patterns |
| [Vikunja](https://github.com/go-vikunja/vikunja) | Self-hosted task management architecture |
| [Logseq](https://github.com/logseq/logseq) | Daily journaling and knowledge graph |
| [Leon AI](https://github.com/leon-ai/leon) | Modular AI assistant architecture |
| [AnythingLLM](https://github.com/Mintplex-Labs/anything-llm) | Document + LLM integration patterns |

### User Research

1. **Conduct interviews** with users of:
   - Notion for life management
   - Roam/Logseq for daily journaling
   - Monday.com/ClickUp for personal projects
   - Replika/Character.AI for emotional AI interaction

2. **Identify pain points**:
   - Where do existing tools fail for personal use?
   - What makes AI assistants feel "goofy" (as you mentioned)?
   - When do nudges feel helpful vs annoying?

---

## Fork vs Fresh Start

### Recommendation: **Fork Khoj**

| Factor | Fork Khoj | Start Fresh |
|--------|-----------|-------------|
| LLM Infrastructure | ✅ Immediate (multi-provider, streaming, tools) | ❌ 3-4 weeks to rebuild |
| Search/Embeddings | ✅ Immediate (pgvector, cross-encoder) | ❌ 2-3 weeks to rebuild |
| Multi-platform | ✅ Immediate (web, mobile, desktop) | ❌ 6-8 weeks to rebuild |
| Conversation Storage | ✅ Immediate | ❌ 1-2 weeks |
| Agent System | ✅ Immediate | ❌ 2-3 weeks |
| Task Management | ❌ Build from scratch | ❌ Build from scratch |
| Journaling | ❌ Build from scratch | ❌ Build from scratch |
| Code Complexity | ⚠️ Large codebase to learn | ✅ Clean slate |
| Upstream Updates | ✅ Can merge new features | N/A |
| Identity | ⚠️ May feel derivative | ✅ Clean identity |

### Fork Strategy

1. **Maintain upstream compatibility** for first 6 months
   - Keep core Khoj features functional
   - Add new systems as additions, not replacements
   - Regularly merge upstream changes

2. **Diverge intentionally** after establishing core features
   - Rename when identity is clear (e.g., "Synapse", "LifeOS", "Compass")
   - Remove unused Khoj features (e.g., WhatsApp integration if not needed)
   - Refactor UI for life management focus

3. **Consider contributing back**
   - Preference learning could benefit upstream Khoj
   - Journal features align with Khoj's personal assistant vision
   - Maintain good open source citizenship

### When to Start Fresh Instead

Start fresh if:
- You find Khoj's architecture fundamentally incompatible with your vision
- The Django + FastAPI hybrid feels too complex
- You prefer a different tech stack (e.g., Go, Rust, or pure Node.js)
- You want a more opinionated, smaller codebase

---

## Open Source Release Strategy

### Project Identity

**Recommended Name Ideas** (check availability):
- **Compass** — Guiding your life direction
- **Anchor** — Grounding and stability
- **Reflect** — Journaling and self-awareness
- **Synapse** — Neural connections, learning, memory
- **Chronicle** — Life story documentation

**Positioning Statement**:
> "An open-source, self-hosted AI life assistant that learns who you are, helps you set and track goals, and gently nudges you toward the life you want to live."

### Pre-Release Checklist

1. **Documentation**
   - [ ] Clear README with vision, features, and screenshots
   - [ ] Installation guide (Docker, manual)
   - [ ] Configuration documentation
   - [ ] API reference
   - [ ] Architecture overview

2. **Demonstration**
   - [ ] Demo video (5-10 min) showing key workflows
   - [ ] Hosted demo instance (optional, for try-before-install)
   - [ ] Screenshot gallery

3. **Community Foundation**
   - [ ] CODE_OF_CONDUCT.md
   - [ ] CONTRIBUTING.md with clear guidelines
   - [ ] Issue templates (bug, feature request)
   - [ ] Discussions or Discord for community

4. **Legal & Licensing**
   - [ ] License file (recommend AGPLv3 to match Khoj, or MIT for max adoption)
   - [ ] Credit Khoj as upstream (required if forking)
   - [ ] Third-party license compliance

### Launch Strategy

#### Phase 1: Soft Launch (Alpha)
- Share with close network for feedback
- Post in r/selfhosted, r/productivity, r/LocalLLaMA
- Focus on early adopters who will tolerate bugs

#### Phase 2: Community Building
- Write blog posts about the journey:
  - "Why I forked Khoj to build a life OS"
  - "How AI learns your preferences over time"
  - "Self-hosted alternatives to Notion for life management"
- Create content for Hacker News, dev.to, Medium

#### Phase 3: Public Launch
- Product Hunt launch
- Hacker News Show HN post
- YouTube review requests to self-hosted community channels:
  - Techno Tim
  - NetworkChuck
  - Jeff Geerling

### Differentiation from Khoj

When presenting, emphasize what makes your fork unique:

| Khoj | Your Project |
|------|--------------|
| "Chat with your documents" | "An AI that knows you and helps you grow" |
| Knowledge retrieval focus | Life management focus |
| Reactive (answers questions) | Proactive (suggests actions) |
| Generic assistant | Personalized life coach |
| Note search | Goal and task tracking |

### Community Engagement

1. **Be responsive** — Answer issues within 24-48 hours initially
2. **Be transparent** — Share roadmap, challenges, and decisions
3. **Be grateful** — Thank contributors publicly
4. **Be patient** — Community building takes months/years

### Sustainability Considerations

If you want this to be a long-term project:

1. **Hosting costs** — Offer self-hosted only initially, add optional cloud tier later
2. **Time commitment** — 10-20 hours/week minimum for active development
3. **Burnout prevention** — Find co-maintainers early
4. **Funding options**:
   - GitHub Sponsors
   - Open Collective
   - Dual licensing (open core + enterprise)
   - Donations

---

## Summary

Khoj provides an excellent foundation for building your AI life management vision:

- **Use immediately**: LLM infrastructure, conversation system, semantic search, agent architecture, multi-platform support
- **Build on top**: Task/goal management, preference learning, journaling, proactive nudges
- **Fork, don't restart**: The engineering value in Khoj is substantial; extend rather than rebuild

The path forward:
1. Fork Khoj
2. Add data models for tasks, goals, preferences, journal entries
3. Build conversational task extraction
4. Implement preference learning pipeline
5. Add journaling with check-ins
6. Create proactive nudge system
7. Polish and prepare for open source launch

This is a 4-6 month project to reach a compelling MVP, with ongoing development thereafter.

---

## Sources & References

### Open Source Task Management Alternatives
- [OpenProject](https://www.openproject.org/)
- [Plane](https://plane.so/)
- [Vikunja](https://vikunja.io/)
- [Taiga](https://www.taiga.io/)
- [Worklenz](https://worklenz.com/)
- [Leantime](https://leantime.io/)

### AI Personal Assistants
- [AnythingLLM](https://anythingllm.com/)
- [Leon AI](https://getleon.ai/)
- [Nextcloud AI Assistant](https://nextcloud.com/blog/first-open-source-ai-assistant/)

### Additional Resources
- [Khoj Repository](https://github.com/khoj-ai/khoj)
- [LlamaIndex for Personal Data](https://www.llamaindex.ai/)
- [LangGraph for Agents](https://github.com/langchain-ai/langgraph)
