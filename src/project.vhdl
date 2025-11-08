
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity tt_um_react_test_saksh156 is
    port (
        ui_in   : in  std_logic_vector(7 downto 0);  -- input pins
        uo_out  : out std_logic_vector(7 downto 0);  -- output pins
        uio_in  : in  std_logic_vector(7 downto 0);  -- bidirectional inputs
        uio_out : out std_logic_vector(7 downto 0);  -- bidirectional outputs
        uio_oe  : out std_logic_vector(7 downto 0);  -- bidirectional enables
        ena     : in  std_logic;                     -- enable
        clk     : in  std_logic;                     -- system clock
        rst_n   : in  std_logic                      -- active-low reset
    );
end tt_um_react_test_saksh156;

architecture Behavioral of tt_um_react_test_saksh156 is

    -- FSM definition
    type state_t is (S_IDLE, S_WAIT_RAND, S_READY, S_MEASURE, S_DONE);
    signal state, next_state : state_t;

    -- Random generator (LFSR)
    signal lfsr        : std_logic_vector(7 downto 0) := (others => '1');
    signal rand_delay  : unsigned(23 downto 0) := (others => '0');
    signal delay_cnt   : unsigned(23 downto 0) := (others => '0');

    -- Timer
    signal timer_cnt   : unsigned(24 downto 0) := (others => '0');

    -- Inputs mapped
    signal start_bn, react_bn, rand_seed : std_logic;
    signal rmode_sel : std_logic_vector(2 downto 0);

    -- LED outputs
    signal led_sim, led_ready, led_false : std_logic;
    signal result : std_logic_vector(4 downto 0);

begin

    ------------------------------------------------------------------------
    -- Input mapping
    ------------------------------------------------------------------------
    start_bn  <= ui_in(0);
    react_bn  <= ui_in(1);
    rand_seed <= ui_in(2);
    rmode_sel <= ui_in(5 downto 3);

    ------------------------------------------------------------------------
    -- LFSR for random delay generation
    ------------------------------------------------------------------------
    process(clk, rst_n)
    begin
        if rst_n = '0' then
            lfsr <= (others => '1');
        elsif rising_edge(clk) then
            if rand_seed = '1' then
                lfsr <= "10101100"; -- re-seed pattern
            else
                -- simple 8-bit LFSR feedback
                lfsr <= lfsr(6 downto 0) & (lfsr(7) xor lfsr(5));
            end if;
        end if;
    end process;

    rand_delay <= unsigned(lfsr & x"0000");  -- expand 8→24 bits

    ------------------------------------------------------------------------
    -- FSM next-state logic
    ------------------------------------------------------------------------
    process(state, start_bn, react_bn, delay_cnt)
    begin
        next_state <= state;
        case state is
            when S_IDLE =>
                if start_bn = '1' then
                    next_state <= S_WAIT_RAND;
                end if;

            when S_WAIT_RAND =>
                if delay_cnt = 0 then
                    next_state <= S_READY;
                end if;

            when S_READY =>
                if react_bn = '1' then
                    next_state <= S_MEASURE;
                end if;

            when S_MEASURE =>
                if react_bn = '1' then
                    next_state <= S_DONE;
                end if;

            when S_DONE =>
                if start_bn = '1' then
                    next_state <= S_IDLE;
                end if;
        end case;
    end process;

    ------------------------------------------------------------------------
    -- FSM state register
    ------------------------------------------------------------------------
    process(clk, rst_n)
    begin
        if rst_n = '0' then
            state <= S_IDLE;
        elsif rising_edge(clk) then
            state <= next_state;
        end if;
    end process;

    ------------------------------------------------------------------------
    -- Random delay counter
    ------------------------------------------------------------------------
    process(clk, rst_n)
    begin
        if rst_n = '0' then
            delay_cnt <= (others => '0');
        elsif rising_edge(clk) then
            case state is
                when S_WAIT_RAND =>
                    if delay_cnt > 0 then
                        delay_cnt <= delay_cnt - 1;
                    else
                        delay_cnt <= rand_delay; -- reload on entry
                    end if;
                when others =>
                    delay_cnt <= rand_delay;
            end case;
        end if;
    end process;

    ------------------------------------------------------------------------
    -- Reaction timer
    ------------------------------------------------------------------------
    process(clk, rst_n)
    begin
        if rst_n = '0' then
            timer_cnt <= (others => '0');
        elsif rising_edge(clk) then
            if state = S_MEASURE then
                timer_cnt <= timer_cnt + 1;
            elsif state = S_IDLE then
                timer_cnt <= (others => '0');
            end if;
        end if;
    end process;

    ------------------------------------------------------------------------
    -- Output logic (LEDs + result)
    ------------------------------------------------------------------------
    led_sim   <= '1' when state = S_WAIT_RAND else '0';
    led_ready <= '1' when state = S_READY else '0';
    led_false <= '1' when (state = S_READY and react_bn = '1' and delay_cnt > 0) else '0';
    result    <= std_logic_vector(timer_cnt(4 downto 0));

    ------------------------------------------------------------------------
    -- Map outputs to TinyTapeout pins
    ------------------------------------------------------------------------
    uo_out(0) <= led_sim;
    uo_out(1) <= led_ready;
    uo_out(2) <= led_false;
    uo_out(7 downto 3) <= result;

    -- Unused bidirectional pins
    uio_out <= (others => '0');
    uio_oe  <= (others => '0');

end Behavioral;
