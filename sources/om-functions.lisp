;============================================================================
; OM-pm2
; pm2 sound analysis and synthesis in OM
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; PM2 Additive Sound Analysis/Systhesis
; Author: Jean Bresson - IRCAM 2005
; update OM6/om# 2016-2018 
;============================================================================

;;;================================================================================================================
;;;================================================================================================================
;;; OM FUNCTIONS

(in-package :om)

;;;================================================================================================================
;;;================================================================================================================

(defmethod! partial-tracking ((sound t) &key
                              begin-t end-t
                              (max-partials 12) (amp-treshold -40)
                              (analysis-type "inharmonic") 
                              (analysis-params '(20 0.0 50 1 3 0.017 50 0.009))
                              (windowsize 4096) (fftsize 4096) (step 256) (windowtype "hanning") 
                              (smoothing-enveloppe  '(0.0 0.0))
                              (out "partials.sdif"))
  :icon :pm2-partials
  :initvals '(nil nil nil 12 -40 "inharmonic" (20 0.0 50 1 3 0.017 50 0.009) 4096 4096 256 "hanning" (0.0 0.0) "partials.sdif")
  :menuins '((5 (("Harmonic" "harmonic") ("Inharmonic" "inharmonic")))
             (10 (("Blackman" "blackman")("Hanning" "hanning")("Hamming" "hamming"))))
  :doc "Tracks sinusoidal partial in an audio file and returns an SDIF sound description file.

- FileName : the pathname of the sound file (Aiff) to be analysed

- begin-t : begin time of analysis (s)

- end-t : end time of analysis (s)

- max-partials : the maximum number of simultaneous partials

- amp-treshold : Amplitude treshold of analysis (-120 -> 0 dB)

- analysis-type : type of analysis (harmonic or inharmonic).

- analysis-params : parameters for tracking                 
                    - for inharmonic partial tracking : 
                           - Relative frequency derivation (mc) (default 20)
                           - Constant frequency derivation (Hz) (default 0.0)
                           - Relative amplitude derivation (%) (default 50)
                           - Source partial neighbors (default 1)
                           - target partial neighbors (>= source partial neighbors) (default 3)
                           - time gap to connect over (s) (default 0.017)
                           - frequency gap to connect over (mc) (default 50)
                           - minimum partial length (s) (default 0.009)
                    - for harmonic partial tracking : an SDIF f0 analysis file (automatically generated if nil)

- windowsize : the number of samples of the analysis window

- fftsize : the number of points of fft

- step : the number of samples between two successive analysis windows

- windowtype : the shape of the analysis window

- smoothing-enveloppe : smoothing enveloppe attack and release times (s) (default '(0.0 0.0))

"
  (om-pm2-lib::pm2-partial-tracking sound 
                                    :begin-t begin-t :end-t end-t
                                    :max-partials max-partials :amp-treshold amp-treshold
                                    :analysis-type analysis-type :analysis-params analysis-params
                                    :windowsize windowsize :fftsize fftsize :step step :windowtype windowtype 
                                    :smoothing-enveloppe smoothing-enveloppe 
                                    :out out))


;;;================================================================================================================
;;;================================================================================================================

(defmethod! chord-seq-analysis ((sound t) &key
                                begin-t end-t
                                markers
                                (max-partials 12) (amp-treshold -40)
                                (analysis-type "averaged-spectrum")
                                (windowsize 4096) (fftsize 4096) (step 256) (windowtype "hanning")
                                (out "chordseqs.sdif"))
  :initvals '(nil nil nil nil 12 -40 "averaged-spectrum" 4096 4096 256 "hanning" "chordseqs.sdif")
  :menuins '((6 (("Averaged Spectrum" "averaged-spectrum") ("Inharmonic Partial Averaging" "inharmonic-partial-averaging")))
             (10 (("Blackman" "blackman")("Hanning" "hanning")("Hamming" "hamming"))))
  :icon :pm2-cseq
  :doc "Tracks sinusoidal chords in an audio file and returns an SDIF sound description file.

- FileName : the pathname of the sound file (Aiff) to be analysed

- begin-t : begin time of analysis (s)

- end-t : end time of analysis (s)

- markers : markers for analysis (text file). If nil, markers of filename are used.

- max-partials : the maximum number of simultaneous partials

- amp-treshold : Amplitude treshold of analysis (-120 -> 0 dB)

- analysis-type : type of analysis ('averaged-spectrum or 'inharmonic-partial-averaging).
                  for inharmonic partial averaging, connect partial connection parameters if needed : 
                           - Relative frequency derivation (mc)
                           - Constant frequency derivation (Hz)
                           - Relative amplitude derivation (%)
                           - Source partial neighbors
                           - Target partial neighbors (>= source partial neighbors)
                           - Time gap to connect over (s)
                           - Frequency gap to connect over (mc)
                           - Minimum partial fragment length (s)
                           - Relative Min. partial length [0.0-1.0]

- windowsize : the number of samples of the analysis window

- fftsize : the number of points of fft

- step : the number of samples between two successive analysis windows

- windowtype : the shape of the analysis window

"

  (om-pm2-lib::pm2-chord-seq-analysis sound :begin-t begin-t :end-t end-t
                                      :markers markers
                                      :max-partials max-partials :amp-treshold amp-treshold
                                      :analysis-type analysis-type
                                      :windowsize windowsize :fftsize fftsize :step step :windowtype windowtype 
                                      :out out))


;;;================================================================================================================
;;;================================================================================================================

(defmethod! pm2-f0 ((sound t) &key
                    begin-t end-t
                    (fund-minfreq 100) (fund-maxfreq 300) (spectrum-maxfreq 3000)
                    (windowsize 4096) (fftsize 4096) (step 256) (windowtype "hanning")
                    (out "f0.sdif"))
            :initvals '(nil nil nil 100 300 3000 4096 4096 256 "hanning" "f0.sdif")
            :menuins '((9 (("Blackman" "blackman")("Hanning" "hanning")("Hamming" "hamming"))))
            :icon :pm2-f0
            :doc "Fundamental frequency estimation.

- FileName : the pathname of the sound file (Aiff) to be analysed

- begin-t : begin time of analysis (s)

- end-t : end time of analysis (s)

- fund-minfreq : min F0 

- fund-maxfreq : max F0  

- spectrum-maxfreq : max analysis frequency

- windowsize : the number of samples of the analysis window

- fftsize : the number of points of fft

- step : the number of samples between two successive analysis windows

- windowtype : the shape of the analysis window

- smoothing-enveloppe : smoothing enveloppe attack and release times (s) (default '(0.0 0.0))

"

            (om-pm2-lib::pm2-f0 sound
                                :begin-t begin-t :end-t end-t
                                :fund-minfreq fund-minfreq :fund-maxfreq fund-maxfreq :spectrum-maxfreq spectrum-maxfreq
                                :windowsize windowsize :fftsize fftsize :step step :windowtype windowtype 
                                :out out))

;;;================================================================================================================
;;;================================================================================================================

(defmethod! pm2-add-synth ((partials t) &key
                           (attack 0.01) (release 0.01)
                           (sr 44100) (res 16) (out "pm2-out.aiff"))
  :initvals '(nil 0.01 0.01 44100 16 "pm2-out.aiff")
  :indoc '("partials" "partials attack time (s)" "partials release time (s)" "sample rate" "resolution" "output pathname")
  :icon :pm2-synth
  :doc "Realizes additive synthesis using pm2.

The input is a simple list of partials provided as an SDIFFILE or SDIF-BUFFER with 1MRK/1TRC frames.

CHORD-SEQ can also be connected and are iternally converted to an SDIF file as well.
"
  (om-pm2-lib::pm2-synthesis partials :attack attack :release release :sr sr :res res :out out))

;;;================================================================================================================
;;;================================================================================================================

(defmethod! pm2-subtract (sound partials &key
                                (attack 0.01) (release 0.01)
                                (sr 44100) (res 16) (out "pm2-out.aiff"))
  :initvals '(nil nil 0.01 0.01 44100 16 "pm2-out.aiff")
  :indoc '("partials" "original sound" 
           "partials attack time (s)" "partials release time (s)" "sample rate" "resolution" "output pathname")
  :icon :pm2-synth
  :doc "Returns residual from <sound> removing <partials> using pm2.

<sound> is aSOUND object or a pathname.

<partials> is a list of partials provided as an SDIFFILE or SDIF-BUFFER with 1MRK/1TRC frames.
A CHORD-SEQ can also be connected and will iternally be converted to an SDIF file.
"
  (om-pm2-lib::pm2-synthesis partials :attack attack :release release :sr sr :res res :out out :sub-from sound))


