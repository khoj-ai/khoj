"""
Knowledge Capsule Lifecycle for KHOJ
Tracks knowledge through SPROUT → GREEN_LEAF → YELLOW_LEAF → RED_LEAF → SOIL phases
"""

from enum import Enum
from typing import Dict, Optional, Any
import time
import json

class Phase(Enum):
    SPROUT = "sprout"
    GREEN_LEAF = "green_leaf"
    YELLOW_LEAF = "yellow_leaf"
    RED_LEAF = "red_leaf"
    SOIL = "soil"

class CapsuleLifecycle:
    """Knowledge lifecycle manager for AI assistants"""
    
    def __init__(self, storage_path: Optional[str] = None):
        self.capsules: Dict[str, dict] = {}
        self.storage_path = storage_path
        if storage_path:
            self._load()
    
    def add(self, capsule_id: str, content: str, 
            priority: str = "P2", metadata: Optional[Dict] = None) -> None:
        """Add new knowledge capsule"""
        self.capsules[capsule_id] = {
            'content': content,
            'priority': priority,
            'confidence': 0.7,
            'phase': Phase.SPROUT,
            'created_at': time.time(),
            'last_accessed': time.time(),
            'access_count': 0,
            'metadata': metadata or {}
        }
        self._save()
    
    def access(self, capsule_id: str) -> bool:
        """Access capsule, boosting confidence"""
        if capsule_id not in self.capsules:
            return False
        
        c = self.capsules[capsule_id]
        c['last_accessed'] = time.time()
        c['access_count'] += 1
        c['confidence'] = min(1.0, c['confidence'] + 0.03)
        
        if c['confidence'] >= 0.8:
            c['phase'] = Phase.GREEN_LEAF
        self._save()
        return True
    
    def decay_all(self) -> None:
        """Apply time-based decay"""
        for c in self.capsules.values():
            decay = {'P0': 0, 'P1': 0.004, 'P2': 0.008}.get(c['priority'], 0.008)
            c['confidence'] = max(0, c['confidence'] - decay)
        self._save()
    
    def get_phase(self, capsule_id: str) -> Optional[str]:
        """Get capsule phase"""
        if capsule_id not in self.capsules:
            return None
        return self.capsules[capsule_id]['phase'].value
    
    def get_status(self) -> Dict:
        """Get all capsules status"""
        phases = {}
        for c in self.capsules.values():
            p = c['phase'].value
            phases[p] = phases.get(p, 0) + 1
        return phases
    
    def _save(self):
        if self.storage_path:
            with open(self.storage_path, 'w') as f:
                json.dump({
                    k: {**v, 'phase': v['phase'].value}
                    for k, v in self.capsules.items()
                }, f)
    
    def _load(self):
        import os
        if os.path.exists(self.storage_path):
            with open(self.storage_path) as f:
                data = json.load(f)
                for k, v in data.items():
                    v['phase'] = Phase(v['phase'])
                self.capsules = data
