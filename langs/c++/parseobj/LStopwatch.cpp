/*!
 *  \file LStopwatch.cpp
 *  \brief Contains the implementation of LStopwatch.
 *  \author Daniel Grigg
 *  \date 2011/02/27
 *  Lexi
 *  Copyright 2011 Daniel Grigg. All rights reserved.
 *
 */

#include "LStopwatch.hpp"

LStopwatch::LStopwatch()
{
#ifdef DARWIN
  mach_timebase_info_data_t info;
  mach_timebase_info(&info);
  m_freqNumer = info.numer;
  m_freqDenom = info.denom;
#endif
  reset();
}

void LStopwatch::reset()
{
#ifdef DARWIN
  m_timeStarted = mach_absolute_time();
#endif
  m_timePaused = 0;
  m_durationPaused = 0;
  m_isPaused = false;
}

void LStopwatch::pause()
{
  if (m_isPaused) return;

  m_isPaused = true;
#ifdef DARWIN
  m_timePaused = mach_absolute_time();
#endif
}

void LStopwatch::play()
{
  if (!m_isPaused) return;

  m_isPaused = false;
#ifdef DARWIN
  m_durationPaused += (mach_absolute_time() - m_timePaused);
#endif
}

float LStopwatch::timeElapsed_s()const
{
#ifdef DARWIN
  uint64_t timeNow;
  if (m_isPaused)
  {
    timeNow = m_timePaused - m_durationPaused;
  }
  else
  {
    timeNow = mach_absolute_time() - m_durationPaused;
  }
  uint64_t dt = timeNow - m_timeStarted;
  uint64_t dt_ns = dt * m_freqNumer / m_freqDenom;
  return dt_ns * 1E-9;
#endif
}
