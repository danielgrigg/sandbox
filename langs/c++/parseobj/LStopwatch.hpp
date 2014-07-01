/*!
 *  \file LStopwatch.hpp
 *  \brief Interface for LStopwatch.
 *  \author Daniel Grigg
 *  \date 2011/02/27
 *  Lexi
 *  Copyright 2011 Daniel Grigg. All rights reserved.
 *
 */

#ifndef LEXI_UTIL_STOPWATCH_HPP
#define LEXI_UTIL_STOPWATCH_HPP

#define DARWIN

#ifdef DARWIN
#include <mach/mach_time.h>
#endif

//! \brief LStopwatch
class LStopwatch
{
  public:
    LStopwatch();

    void reset();

    float timeElapsed_s()const;

    void pause();
    void play();
  private:
#ifdef DARWIN
    uint64_t m_timeStarted;  // Mach-time-units
    uint32_t m_freqDenom;
    uint32_t m_freqNumer;
    uint64_t m_timePaused;
    uint64_t m_durationPaused;
    bool m_isPaused;
#endif
};

#endif // ifndef LEXI_UTIL_STOPWATCH_HPP
