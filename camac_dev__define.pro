pro camac_dev::set_cnaf, c, n, a, f
    if (n_elements(c) ne 0) then self.c = c
    if (n_elements(n) ne 0) then self.n = n
    if (n_elements(a) ne 0) then self.a = a
    if (n_elements(f) ne 0) then self.f = f
end

pro camac_dev::control, init=init, clear=clear, setinh=setinh, clrinh=clrinh, $
                        online=online
    self->set_cnaf, c, n, a, f
    func = 3
    if (keyword_set(init)) then func = 0
    if (keyword_set(clean)) then func = 1
    if (keyword_set(setinh)) then func = 2
    if (keyword_set(clrinh)) then func = 3
    if (keyword_set(online)) then func = 4
    status_array = lonarr(10)
    status = call_external(self.dll_name, 'cactrl_idl', self.handle, $
                           self.c, func, status_array)
end

pro camac_dev::single_transfer, data, status_array, short=short, $
                                c=c, n=n, a=a, f=f
    self->set_cnaf, c, n, a, f
    if (self.f lt 16) or (self.f gt 23) then data = 0L
    status_array = lonarr(10)
    if (keyword_set(short)) then begin
        data = fix(data) 
        status = call_external(self.dll_name, 'cam16_idl', self.handle, $
                           self.c, self.n, self.a, self.f, data, status_array)
    endif else begin
        data = long(data)
        status = call_external(self.dll_name, 'cam24_idl', self.handle, $
                           self.c, self.n, self.a, self.f, data, status_array)
    endelse
end

pro camac_dev::block_transfer, data, status_array, count=count, short=short, $
                      qstp=qstp, qign=qign, qrpt=qrpt, qscn=qscn, $
                      c=c, n=n, a=a, f=f
    self->set_cnaf, c, n, a, f
    if (n_elements(count) eq 0) then count = 1024L
    if (self.f lt 16) or (self.f gt 23) then data = lonarr(count)
    mode = '10'x
    if (keyword_set(qstp)) then mode = '00'x
    if (keyword_set(qign)) then mode = '08'x
    if (keyword_set(qrpt)) then mode = '10'x
    if (keyword_set(qscn)) then mode = '18'x
    status_array = lonarr(10)
    if (keyword_set(short)) then begin
        if (self.f lt 16) or (self.f gt 23) then data = intarr(count) $
            else data = fix(data) 
        status = call_external(self.dll_name, 'cab16_idl', self.handle, $
                           self.c, self.n, self.a, self.f, $
                           fix(mode), data, long(count), status_array)
    endif else begin
        if (self.f lt 16) or (self.f gt 23) then data = lonarr(count) $
            else data = long(data)
        status = call_external(self.dll_name, 'cab24_idl', self.handle, $
                           self.c, self.n, self.a, self.f, $
                           fix(mode), data, long(count), status_array)
    endelse
end

function camac_dev::init, device=device, c=c, n=n, a=a, f=f


    self.dll_name = 'camac_idl.dll'
    self->set_cnaf, c, n, a, f
    if (n_elements(device) eq 0) then device = 'kpa00'
    handle = 0L
    device = [byte(device),0B]  ; Need to add a null
    status_array = lonarr(10)
    status = call_external(self.dll_name, 'caopen_idl', $
                           handle, device, status_array)
    self.handle = handle
    if (status eq '21F50001'X) then return, 1 else begin
        print, format='(a, z)', 'Error in camac_dev::init, status = ', status
        return, 0
    endelse
end

pro camac_dev__define
    camac_dev = {camac_dev, dll_name: '', handle: 0L, c: 0, n: 0, a: 0, f: 0}
end
