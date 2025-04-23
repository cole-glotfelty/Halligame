"""Serializes and deserializes game state for client/server communication.

Written by Will Cordray, Cole Glotfelty, Michael Daniels <2025-03-29>
Last modified by: Cole Glotfelty <2025-04-14>

Changelog:
Michael Daniels <2025-04-12> - More documentation.
Cole Glotfelty <2025-04-14> - Added Documentation
Michael Daniels <2025-04-03> - Switched to pickle for serialization
"""

import pickle
from threading import Lock
from typing import Any


class GameState:
    """A Thread-safe implementation of a dictionary.

    Used for storing/managing game state.
    """

    def __init__(self, args: dict | None = None) -> None:
        """Initialize this instance."""
        self.__lock: Lock = Lock()
        """Protects internal state."""
        self.__objects: dict[Any, Any]
        """The object store."""
        if args is None:
            self.__objects = {}
        else:
            self.__objects = args

    def serialize(self) -> bytes:
        """Serialize the state dictionary (into binary) using pickle."""
        with self.__lock:
            return pickle.dumps(self.__objects)

    def deserialize(self, state: bytes) -> None:
        """Deserlialize the state dictionary from binary using pickle.

        Args:
            state: pickled series of bytes
        """
        with self.__lock:
            self.__objects = pickle.loads(state)

    def getValue(self, key: Any) -> Any:
        """Get the value stored at the given key."""
        with self.__lock:
            return self.__objects[key]

    def setValue(self, key: Any, value: Any) -> None:
        """Set the value stored at the given key to value."""
        with self.__lock:
            self.__objects[key] = value
