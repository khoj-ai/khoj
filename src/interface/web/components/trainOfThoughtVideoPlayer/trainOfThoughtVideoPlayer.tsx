"use client";

import React, { useState, useRef, useEffect } from "react";
import { Play, Pause, FastForward, Rewind } from "@phosphor-icons/react";
import styles from "./trainOfThoughtVideoPlayer.module.css";

interface TrainOfThoughtFrame {
    text: string;
    image?: string;
    timestamp: number;
}

interface TrainOfThoughtVideoPlayerProps {
    frames: TrainOfThoughtFrame[];
    autoPlay?: boolean;
    playbackSpeed?: number;
}

export default function TrainOfThoughtVideoPlayer({
    frames,
    autoPlay = true,
    playbackSpeed = 1000, // ms per frame
}: TrainOfThoughtVideoPlayerProps) {
    const [currentFrameIndex, setCurrentFrameIndex] = useState(0);
    const [isPlaying, setIsPlaying] = useState(autoPlay);
    const [isAutoTracking, setIsAutoTracking] = useState(true);
    const intervalRef = useRef<NodeJS.Timeout | null>(null);

    // Auto-advance to latest frame when new frames are added
    useEffect(() => {
        if (isAutoTracking && frames.length > 0) {
            setCurrentFrameIndex(frames.length - 1);
        }
    }, [frames.length, isAutoTracking]);

    // Handle playback
    useEffect(() => {
        if (isPlaying && frames.length > 1) {
            intervalRef.current = setInterval(() => {
                setCurrentFrameIndex((prev) => {
                    const next = prev + 1;
                    if (next >= frames.length) {
                        setIsPlaying(false);
                        return prev;
                    }
                    return next;
                });
            }, playbackSpeed);
        } else {
            if (intervalRef.current) {
                clearInterval(intervalRef.current);
                intervalRef.current = null;
            }
        }

        return () => {
            if (intervalRef.current) {
                clearInterval(intervalRef.current);
            }
        };
    }, [isPlaying, frames.length, playbackSpeed]);

    const currentFrame = frames[currentFrameIndex];

    const handleSeek = (index: number) => {
        setCurrentFrameIndex(index);
        setIsAutoTracking(false);
        setIsPlaying(false);
    };

    const handlePlay = () => {
        setIsPlaying(!isPlaying);
        setIsAutoTracking(false);
    };

    const handlePrevious = () => {
        if (currentFrameIndex > 0) {
            setCurrentFrameIndex(currentFrameIndex - 1);
            setIsAutoTracking(false);
            setIsPlaying(false);
        }
    };

    const handleNext = () => {
        if (currentFrameIndex < frames.length - 1) {
            setCurrentFrameIndex(currentFrameIndex + 1);
            setIsAutoTracking(false);
            setIsPlaying(false);
        }
    };

    const handleAutoTrack = () => {
        setIsAutoTracking(true);
        setCurrentFrameIndex(frames.length - 1);
        setIsPlaying(false);
    };

    if (!frames.length) {
        return null;
    }

    return (
        <div className={styles.videoPlayer}>
            <div className={styles.screen}>
                {currentFrame?.image && (
                    <img
                        src={currentFrame.image}
                        alt={`Train of thought frame ${currentFrameIndex + 1}`}
                        className={styles.screenImage}
                    />
                )}
                <div className={styles.textOverlay}>
                    <div className={styles.thoughtText}>{currentFrame?.text}</div>
                </div>
            </div>

            <div className={styles.controls}>
                <div className={styles.timeline}>
                    <input
                        type="range"
                        min={0}
                        max={Math.max(0, frames.length - 1)}
                        value={currentFrameIndex}
                        onChange={(e) => handleSeek(parseInt(e.target.value))}
                        className={styles.timelineSlider}
                    />
                    <div className={styles.frameMarkers}>
                        {frames.map((frame, index) => (
                            <div
                                key={index}
                                className={`${styles.frameMarker} ${
                                    frame.image ? styles.hasImage : styles.textOnly
                                } ${index === currentFrameIndex ? styles.active : ""}`}
                                onClick={() => handleSeek(index)}
                                title={`Frame ${index + 1}: ${frame.text.slice(0, 50)}...`}
                            />
                        ))}
                    </div>
                </div>

                <div className={styles.controlButtons}>
                    <button
                        onClick={handlePrevious}
                        disabled={currentFrameIndex === 0}
                        title="Previous frame"
                        className={styles.controlButton}
                    >
                        <Rewind size={16} />
                    </button>

                    <button
                        onClick={handlePlay}
                        disabled={frames.length <= 1}
                        title={isPlaying ? "Pause" : "Play"}
                        className={styles.controlButton}
                    >
                        {isPlaying ? <Pause size={16} /> : <Play size={16} />}
                    </button>

                    <button
                        onClick={handleNext}
                        disabled={currentFrameIndex === frames.length - 1}
                        title="Next frame"
                        className={styles.controlButton}
                    >
                        <FastForward size={16} />
                    </button>

                    <button
                        onClick={handleAutoTrack}
                        className={`${styles.controlButton} ${isAutoTracking ? styles.active : ""}`}
                        title="Auto-track latest"
                    >
                        Live
                    </button>
                </div>

                <div className={styles.frameInfo}>
                    <span>
                        {currentFrameIndex + 1} / {frames.length}
                    </span>
                </div>
            </div>
        </div>
    );
}
