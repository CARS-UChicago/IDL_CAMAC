;*****************************************************************************
pro camac_scaler::wait, dwell_time
;+
; NAME:
;       CAMAC_SCALER::WAIT
;
; PURPOSE:
;       This function waits for counting on the scaler to complete.
;
; CATEGORY:
;       CAMAC device class library.
;
; CALLING SEQUENCE:
;
;       scaler->WAIT
;
; INPUTS:
;       None
;
; OUTPUTS:
;       None
;
; PROCEDURE:
;	This routine simply tests whether the scaler is done counting. If
;       it is then the routine returns.  If it is not it waits for 10% of the
;       counting time or 0.1 second (whichever is less) and tries again.
;
; EXAMPLE:
;       scaler = obj_new('camac_scaler', clock_c, clock_n, $
;                                         scaler_cs, scaler_ns, scaler_as)
;       scaler->start, 10.  ; Start counting for 10 seconds.
;       scaler->wait        ; Wait for counting to complete
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, March 13, 1998
;-

wait_time = self.preset_time < 0.1 ; Wait for 10% of dwell time or 0.1 second,
                                    ; whichever is less

while (1) do begin
    wait, wait_time
    time = self->read_clock()
    if (time eq 0.) then return
endwhile
end


;*****************************************************************************
function camac_scaler::read, scaler
;+
; NAME:
;       CAMAC_SCALER::READ
;
; PURPOSE:
;       This function returns the counts on the scaler.  It can either return
;       the counts on a single scaler channel or on all of the scaler channels.
;
; CATEGORY:
;       CAMAC device class library.
;
; CALLING SEQUENCE:
;
;       Result = scaler->READ(Channel)
;
; OPTIONAL INPUTS:
;       Channel:  If a channel is specified then only the counts on this
;                 scaler channel are returned.  By default the counts on all
;                 scaler channels are returned.  
;
; OUTPUTS:
;       Returns the counts.  This can be a single number if the optional
;       Channel input was specified, or an array of counts if Channel was not
;       specified.
;
; EXAMPLE:
;       scaler = obj_new('camac_scaler', clock_c, clock_n, $
;                                         scaler_cs, scaler_ns, scaler_as)
;       scaler->START, 10.          ; Start counting for 10 seconds.
;       scaler->WAIT                ; Wait for counting to complete
;       counts = scaler->READ()     ; Read the counts on all of the channels
;       counts = scaler->READ(0)    ; Read the counts on the first channel,
;                                   ; which is the preset clock.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, March 13, 1998
;-
; This function reads the counts on one of the scalers or all of them
    if (n_elements(scaler) ne 0) then begin
        self.scaler_devs[scaler]->single_transfer, counts, status, f=0
    endif else begin
        counts = lonarr(self.n_scalers)
        for i=0,self.n_scalers-1 do begin
            self.scaler_devs[i]->single_transfer, temp, status, f=0
            counts[i]=temp
        endfor
    endelse
    return, counts
end

;*****************************************************************************
function camac_scaler::read_clock
;+
; NAME:
;       CAMAC_SCALER::READ_CLOCK
;
; PURPOSE:
;       This function returns the remaining time on the scaler clock in
;       seconds.
;
; CATEGORY:
;       CAMAC device class library.
;
; CALLING SEQUENCE:
;
;       Result = scaler->READ_CLOCK()
;
; OUTPUTS:
;       Returns the remaining counting time in seconds, 0. if the clock is done
;       counting.
;
; EXAMPLE:
;       scaler = obj_new('camac_scaler', clock_c, clock_n, $
;                                         scaler_cs, scaler_ns, scaler_as)
;       scaler->START, 10.          ; Start counting for 10 seconds.
;       time = scaler->READ_CLOCK() ; Read the clock
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, March 13, 1998
;-
    self.clock_dev->single_transfer, ticks, status, /short, f=0
    ticks = not(ticks)  ; Ones complement
    time = (long(ticks) and '0000ffff'xL)/self.freq
    return, time
end

;*****************************************************************************
pro camac_scaler::start, preset_time, actual_time
;+
; NAME:
;       CAMAC_SCALER::START
;
; PURPOSE:
;       This function starts the scaler counting.
;
; CATEGORY:
;       CAMAC device class library.
;
; CALLING SEQUENCE:
;
;       scaler->START, Time, Actual_Time
;
; OPTIONAL INPUTS:
;   Time:  The preset counting time in seconds.  If this input parameter 
;          is not specified then the preset time of the scaler is not 
;          changed.
;
; OUTPUTS:
;   Actual_Time: The actual counting time in seconds, which may differ from
;                the requested time because of the finite resolution of the
;                clock
;
; PROCEDURE:
;   This routine set the real-time clock to count for SECS seconds.  It 
;   computes the optimum frequency division to use, and programs this into
;   the RTC-018.
;
; EXAMPLE:
;       scaler = obj_new('camac_scaler', '13IDC:scaler1')
;       scaler->START, 10.          ; Start counting for 10 seconds.
;       scaler->WAIT                ; Wait for counting to complete
;       counts = scaler->READ()     ; Read the counts on all of the channels
;                                   ; which is the preset clock.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, March 13, 1998
;-

;   Clear the scalers
    for i=0,self.n_scalers-1 do begin
        self.scaler_devs[i]->single_transfer, 0, status, f=9
    endfor

;   If the desired time interval does not change this procedure can be called
;   without the Time parameter, because the frequency division and preset 
;   counter need only be programmed once.

    if (n_elements(preset_time) ne 0) then self.preset_time = preset_time

;   Compute frequency divison required to obtain this time. This assumes
;   we are using the internal oscillator with a frequency of 2**18 Hz.
    dsecs = double(self.preset_time)
    div = 2.^18 * dsecs
    actual_time = long(div+0.5) / 2.^18
    allfrq = [262144., 32768., 4096., 512., 64., 8., 1.]
    alldiv = [1, 2, 4, 8, 16, 32, 64]

;   Division to be programmed is one less than requested division (since
;   programmed divisor of 1 produces actual frequency division of 2 etc.
    div = div - 1.d0
    if ((div le 1.0) or (div gt 65535.D0 * 8.^6)) then begin
        message, 'Divisor out of range in camac_scaler::start'
    endif

;   Step through frequency divisors, find the smallest one which will work
    for n=0, 6 do begin
        if (div le (65535.D0 * 8.D0^(n))) then goto, found_div
    endfor

    found_div:
;   Get frequency divider mask
    idiv = alldiv(n)

;   Compute value to load into preset counter
    icount = (div / 8.D0^(n)) + 0.5

;   Write out frequency divisor to clock
    self.clock_dev->single_transfer, idiv, status, /short, a=1, f=16

;   Write out preset counter - only need to send 16 bits.
    self.clock_dev->single_transfer, icount, status, /short, a=0, f=16


    ; Save the clock frequency
    self.freq = allfrq(n)

end


;*****************************************************************************
pro camac_scaler::stop
;+
; NAME:
;       CAMAC_SCALER::STOP
;
; PURPOSE:
;       This function stops the scaler immediately from counting.
;
; CATEGORY:
;       CAMAC device class library.
;
; CALLING SEQUENCE:
;
;       scaler->SCALER_STOP
;
; INPUTS:
;       None
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       scaler = OBJ_NEW('camac_scaler', Clock_C, Clock_N, $
;                                        Scaler_C, Scaler_N, Scaler_A)
;       scaler->START               ; Start counting 
;       scaler->STOP                ; Stop immediately
;       counts = scaler->READ()     ; Read the counts on all of the channels
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, March 13, 1998
;-

;   For now just count for 10 microseconds.  This is not pretty, but it
;   essentially stops the clock immediately.
    self->start, 1.e-5  
end



;*****************************************************************************
function camac_scaler::get_title, channel
;+
; NAME:
;       CAMAC_SCALER::GET_TITLE
;
; PURPOSE:
;       This function returns the .NMx field of the CAMAC scaler record. This
;       is typically a short description of the scaler input.
;
; CATEGORY:
;       CAMAC device class library.
;
; CALLING SEQUENCE:
;
;       Result = scaler->GET_TITLE(Channel)
;
; INPUTS:
;       None:
;
; OPTIONAL INPUTS:
;       Channel:  If a channel is specified then only the title of this
;                 scaler channel is returned.  By default the titles of all
;                 scaler channels are returned.
;
; OUTPUTS:
;       This function returns the titles of the scaler channels.
;
; EXAMPLE:
;       scaler = OBJ_NEW('camac_scaler', Clock_C, Clock_N, $
;                                        Scaler_C, Scaler_N, Scaler_A)
;       print, scaler->get_title(1)
;       Photodiode
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, March 13, 1998
;-
    if (n_elements(channel) eq 0) then begin
        return, self.title[0:self.n_scalers-1]
    endif else begin
        return, self.title[channel]
    endelse
end

;*****************************************************************************
pro camac_scaler::set_title, channel, title
;+
; NAME:
;       CAMAC_SCALER::SET_TITLE
;
; PURPOSE:
;       This procedure sets the .NMx field of the CAMAC scaler record. This
;       is typically a short description of the scaler input.
;
; CATEGORY:
;       CAMAC device class library.
;
; CALLING SEQUENCE:
;
;       scaler->SET_TITLE, Channel, Title
;
; INPUTS:
;       Channel:  The scaler channel whose title is to be set.  This is a
;                 number in the range 0 - n_scalers-1
;       Title:    The title string.
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       scaler = OBJ_NEW('camac_scaler', Clock_C, Clock_N, $
;                                        Scaler_C, Scaler_N, Scaler_A)
;       scaler->set_title, 1, 'Photodiode'
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, March 13, 1998
;-
    self.title[channel] = title
end


;*****************************************************************************
function camac_scaler::init, Clock_C, Clock_N, Scaler_C, Scaler_N, Scaler_A

;+
; NAME:
;       CAMAC_SCALER::INIT
;
; PURPOSE:
;       This is the initialization code which is invoked when a new object of
;       type CAMAC_SCALER is created.  It cannot be called directly, but only
;       indirectly by the IDL OBJ_NEW() function.
;
; CATEGORY:
;       CAMAC device class library.
;
; CALLING SEQUENCE:
;       Result = OBJ_NEW('camac_scaler', Clock_C, Clock_N, $
;                                        Scaler_C, Scaler_N, Scaler_A)
;
; INPUTS:
;   Clock_C     Crate number for the real-time clock
;   Clock_N     Slot number for the real-time clock
;   Scaler_C    Crate numbers for the scalers
;   Scaler_N    Slot numbers for the scalers
;   Scaler_A    Subaddresses for the scalers
;
;   Note: Scaler_C, Scaler_N, and Scaler_A are normally arrays, to define
;         a scaler object with multiple scaler inputs.  The arrays must all
;         have the same dimensions.
;
; OUTPUTS:
;       This function returns a status to indicate whether it was able to
;       establish communication with the specified CAMAC scaler.
;       This status is 1 for success, 0 for failure.  This status is
;       passed back indirectly to the routine which calls OBJ_NEW().  OBJ_NEW
;       will return a valid object pointer if this routine succeeds, and will
;       return a NULL object pointer if this routine fails.  The user should
;       test the return value of OBJ_NEW() with the IDL function OBJ_VALID().
;
; RESTRICTIONS:
;       This routine cannot be called directly.  It is called indirectly when
;       creating a new object of class CAMAC_SCALER by the IDL OBJ_NEW()
;       function.
;
; EXAMPLE:
;       This example creates a scaler object with a real-time clock in slot
;       12 and a 4-channel scaler in slot 13, all in crate 1.
;       scaler = obj_new('camac_scaler', 1, 12, $
;                                        [1,1,1,1], [13,13,13,13], [0,1,2,3])
;       if (OBJ_VALID(scaler)) then print, 'It worked!'
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, March 13, 1998
;-
    self.clock_dev = obj_new('CAMAC_DEV', C=Clock_C, N=Clock_N)
    if (not obj_valid(self.clock_dev)) then return, 0
    self.n_scalers = n_elements(Scaler_C)
    for i=0, self.n_scalers-1 do begin
        self.scaler_devs[i] = obj_new('CAMAC_DEV', C=Scaler_C[i], $
                                        N=Scaler_N[i], A=Scaler_A[i])
        if (not obj_valid(self.scaler_devs[i])) then return, 0
    endfor
    return, 1
end

;*****************************************************************************
pro camac_scaler__define
;+
; NAME:
;       CAMAC_SCALER__DEFINE
;
; PURPOSE:
;       This is the definition code which is invoked when a new object of
;       type CAMAC_SCALER is created.  It cannot be called directly, but only
;       indirectly by the IDL OBJ_NEW() function,
;
; CATEGORY:
;       CAMAC device class library.
;
; CALLING SEQUENCE:
;       Scaler = obj_new('camac_scaler', Clock_C, Clock_N, $
;                                        Scaler_C, Scaler_N, Scaler_A)
;
; INPUTS:
;   Clock_C     Crate number for the real-time clock
;   Clock_N     Slot number for the real-time clock
;   Scaler_C    Crate numbers for the scalers
;   Scaler_N    Slot numbers for the scalers
;   Scaler_A    Subaddresses for the scalers
;
;   Note: Scaler_C, Scaler_N, and Scaler_A are normally arrays, to define
;         a scaler object with multiple scaler inputs.  The arrays must all
;         have the same dimensions.
;
; OUTPUTS:
;       None (but see CAMAC_SCALER::INIT)
;
; RESTRICTIONS:
;       This routine cannot be called directly.  It is called indirectly when
;       creating a new object of class CAMAC_SCALER by the IDL OBJ_NEW()
;       function.
;
; EXAMPLE:
;       This example creates a scaler object with a real-time clock in slot
;       12 and a 4-channel scaler in slot 13, all in crate 1.
;       scaler = obj_new('camac_scaler', 1, 12, $
;                                        [1,1,1,1], [13,13,13,13], [0,1,2,3])
;       if (OBJ_VALID(scaler)) then print, 'It worked!'
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, March 13, 1998
;-
    MAX_SCALERS = 16
    camac_scaler = { camac_scaler, n_scalers: 0, preset_time: 0., freq: 0., $
                        clock_dev: obj_new(), $
                        scaler_devs: objarr(MAX_SCALERS), $
                        title: strarr(MAX_SCALERS)}

end
