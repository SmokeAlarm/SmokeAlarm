-record(app_conf, {
        % child_* : Configuration of SmokeAlarm's superivsion tree.
            % Restart freq for all node supervised processes
            %   (See: OTP doc/design_principles/sup_princ.html#frequency)
            child_restart_freq_maxr = 1,
            child_restart_freq_maxt = 30,
            % Shutdown method all supervised processes
            %   (See: OTP doc/design_principles/sup_princ.html#spec)
            %   NB: Don't use infinity!
            child_shutdown_timeout = 5000,    
            % gen_server/gen_fsm start_link Options for all supervised processes
            %   i.e. {debug, _}, {timeout, _}, {spawn_opt, _}
            %   (See: OTP doc for gen_server:start_link, gen_fsm:start_link)
            child_otp_start_opts = [],
        % dtor_* : Global configuration of SmokeAlarm's detectors.
            % Auto-remove frequency for detectors
            %   How many MaxR errors in MaxTu time in a any reactor callback
            %   cause it's detector to automatically remove the reactor.
            %   NB: - 0 in either value means never
            %       - MaxTu is in MICROSECONDS.
            dtor_faulty_freq_maxr = 1,
            dtor_faulty_freq_maxtu = 30*1000000
        }).
        
-record(trace_conf, {
            start_viewer = false,
            start_tracer = undefined,
            trace_flags = undefined, 
            enable_trace_me = false,
            enable_trace_calls = false
        }).
        
