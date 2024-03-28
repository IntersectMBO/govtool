--
-- PostgreSQL database dump
--

-- Dumped from database version 15.3 (Debian 15.3-1.pgdg120+1)
-- Dumped by pg_dump version 15.3 (Debian 15.3-1.pgdg120+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: public; Type: SCHEMA; Schema: -; Owner: postgres
--

-- *not* creating schema, since initdb creates it


ALTER SCHEMA public OWNER TO postgres;

--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA public IS '';


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: anchor_offline_data; Type: TABLE; Schema: public; Owner: test
--

CREATE TABLE public.anchor_offline_data (
    id integer NOT NULL,
    voting_anchor_id bigint NOT NULL,
    hash text NOT NULL,
    json jsonb NOT NULL
);


ALTER TABLE public.anchor_offline_data OWNER TO test;

--
-- Name: anchor_offline_data_id_seq; Type: SEQUENCE; Schema: public; Owner: test
--

CREATE SEQUENCE public.anchor_offline_data_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.anchor_offline_data_id_seq OWNER TO test;

--
-- Name: anchor_offline_data_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: test
--

ALTER SEQUENCE public.anchor_offline_data_id_seq OWNED BY public.anchor_offline_data.id;


--
-- Name: delegation_vote; Type: TABLE; Schema: public; Owner: test
--

CREATE TABLE public.delegation_vote (
    id integer NOT NULL,
    stake_addr text NOT NULL,
    drep_id integer,
    abstain boolean,
    no_confidence boolean,
    CONSTRAINT chk_only_one_delegation_type CHECK ((num_nonnulls(drep_id, abstain, no_confidence) = 1))
);


ALTER TABLE public.delegation_vote OWNER TO test;

--
-- Name: delegation_vote_drep_id_seq; Type: SEQUENCE; Schema: public; Owner: test
--

CREATE SEQUENCE public.delegation_vote_drep_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.delegation_vote_drep_id_seq OWNER TO test;

--
-- Name: delegation_vote_drep_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: test
--

ALTER SEQUENCE public.delegation_vote_drep_id_seq OWNED BY public.delegation_vote.drep_id;


--
-- Name: delegation_vote_id_seq; Type: SEQUENCE; Schema: public; Owner: test
--

CREATE SEQUENCE public.delegation_vote_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.delegation_vote_id_seq OWNER TO test;

--
-- Name: delegation_vote_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: test
--

ALTER SEQUENCE public.delegation_vote_id_seq OWNED BY public.delegation_vote.id;


--
-- Name: drep; Type: TABLE; Schema: public; Owner: test
--

CREATE TABLE public.drep (
    id integer NOT NULL,
    drep_raw text NOT NULL,
    voting_anchor_id bigint,
    is_active boolean
);


ALTER TABLE public.drep OWNER TO test;

--
-- Name: drep_id_seq; Type: SEQUENCE; Schema: public; Owner: test
--

CREATE SEQUENCE public.drep_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.drep_id_seq OWNER TO test;

--
-- Name: drep_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: test
--

ALTER SEQUENCE public.drep_id_seq OWNED BY public.drep.id;


--
-- Name: governance_action; Type: TABLE; Schema: public; Owner: test
--

CREATE TABLE public.governance_action (
    id text NOT NULL,
    type text NOT NULL,
    details text NOT NULL,
    expiry_date timestamp with time zone NOT NULL,
    voting_anchor_id bigint NOT NULL
);


ALTER TABLE public.governance_action OWNER TO test;

--
-- Name: voting_anchor; Type: TABLE; Schema: public; Owner: test
--

CREATE TABLE public.voting_anchor (
    id integer NOT NULL,
    url character varying(2048) NOT NULL,
    doc_hash text NOT NULL
);


ALTER TABLE public.voting_anchor OWNER TO test;

--
-- Name: voting_anchor_id_seq; Type: SEQUENCE; Schema: public; Owner: test
--

CREATE SEQUENCE public.voting_anchor_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.voting_anchor_id_seq OWNER TO test;

--
-- Name: voting_anchor_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: test
--

ALTER SEQUENCE public.voting_anchor_id_seq OWNED BY public.voting_anchor.id;


--
-- Name: voting_procedure; Type: TABLE; Schema: public; Owner: test
--

CREATE TABLE public.voting_procedure (
    id integer NOT NULL,
    governance_action_id text NOT NULL,
    vote text NOT NULL,
    voting_anchor_id bigint NOT NULL,
    drep_id bigint NOT NULL
);


ALTER TABLE public.voting_procedure OWNER TO test;

--
-- Name: voting_procedure_id_seq; Type: SEQUENCE; Schema: public; Owner: test
--

CREATE SEQUENCE public.voting_procedure_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 2147483647
    CACHE 1;


ALTER TABLE public.voting_procedure_id_seq OWNER TO test;

--
-- Name: voting_procedure_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: test
--

ALTER SEQUENCE public.voting_procedure_id_seq OWNED BY public.voting_procedure.id;


--
-- Name: anchor_offline_data id; Type: DEFAULT; Schema: public; Owner: test
--

ALTER TABLE ONLY public.anchor_offline_data ALTER COLUMN id SET DEFAULT nextval('public.anchor_offline_data_id_seq'::regclass);


--
-- Name: delegation_vote id; Type: DEFAULT; Schema: public; Owner: test
--

ALTER TABLE ONLY public.delegation_vote ALTER COLUMN id SET DEFAULT nextval('public.delegation_vote_id_seq'::regclass);


--
-- Name: drep id; Type: DEFAULT; Schema: public; Owner: test
--

ALTER TABLE ONLY public.drep ALTER COLUMN id SET DEFAULT nextval('public.drep_id_seq'::regclass);


--
-- Name: voting_anchor id; Type: DEFAULT; Schema: public; Owner: test
--

ALTER TABLE ONLY public.voting_anchor ALTER COLUMN id SET DEFAULT nextval('public.voting_anchor_id_seq'::regclass);


--
-- Name: voting_procedure id; Type: DEFAULT; Schema: public; Owner: test
--

ALTER TABLE ONLY public.voting_procedure ALTER COLUMN id SET DEFAULT nextval('public.voting_procedure_id_seq'::regclass);


--
-- Name: anchor_offline_data anchor_offline_data_pkey; Type: CONSTRAINT; Schema: public; Owner: test
--

ALTER TABLE ONLY public.anchor_offline_data
    ADD CONSTRAINT anchor_offline_data_pkey PRIMARY KEY (id);


--
-- Name: delegation_vote delegation_vote_pkey; Type: CONSTRAINT; Schema: public; Owner: test
--

ALTER TABLE ONLY public.delegation_vote
    ADD CONSTRAINT delegation_vote_pkey PRIMARY KEY (id);


--
-- Name: drep drep_pkey; Type: CONSTRAINT; Schema: public; Owner: test
--

ALTER TABLE ONLY public.drep
    ADD CONSTRAINT drep_pkey PRIMARY KEY (id);


--
-- Name: governance_action governance_action_pkey; Type: CONSTRAINT; Schema: public; Owner: test
--

ALTER TABLE ONLY public.governance_action
    ADD CONSTRAINT governance_action_pkey PRIMARY KEY (id);


--
-- Name: voting_anchor voting_anchor_pkey; Type: CONSTRAINT; Schema: public; Owner: test
--

ALTER TABLE ONLY public.voting_anchor
    ADD CONSTRAINT voting_anchor_pkey PRIMARY KEY (id);


--
-- Name: voting_procedure voting_procedure_pkey; Type: CONSTRAINT; Schema: public; Owner: test
--

ALTER TABLE ONLY public.voting_procedure
    ADD CONSTRAINT voting_procedure_pkey PRIMARY KEY (id);


--
-- Name: delegation_vote drep_id; Type: FK CONSTRAINT; Schema: public; Owner: test
--

ALTER TABLE ONLY public.delegation_vote
    ADD CONSTRAINT drep_id FOREIGN KEY (drep_id) REFERENCES public.drep(id);


--
-- Name: voting_procedure drep_id; Type: FK CONSTRAINT; Schema: public; Owner: test
--

ALTER TABLE ONLY public.voting_procedure
    ADD CONSTRAINT drep_id FOREIGN KEY (drep_id) REFERENCES public.drep(id);


--
-- Name: voting_procedure governance_action_id; Type: FK CONSTRAINT; Schema: public; Owner: test
--

ALTER TABLE ONLY public.voting_procedure
    ADD CONSTRAINT governance_action_id FOREIGN KEY (governance_action_id) REFERENCES public.governance_action(id);


--
-- Name: drep voting_anchor_id; Type: FK CONSTRAINT; Schema: public; Owner: test
--

ALTER TABLE ONLY public.drep
    ADD CONSTRAINT voting_anchor_id FOREIGN KEY (voting_anchor_id) REFERENCES public.voting_anchor(id);


--
-- Name: governance_action voting_anchor_id; Type: FK CONSTRAINT; Schema: public; Owner: test
--

ALTER TABLE ONLY public.governance_action
    ADD CONSTRAINT voting_anchor_id FOREIGN KEY (voting_anchor_id) REFERENCES public.voting_anchor(id);


--
-- Name: voting_procedure voting_anchor_id; Type: FK CONSTRAINT; Schema: public; Owner: test
--

ALTER TABLE ONLY public.voting_procedure
    ADD CONSTRAINT voting_anchor_id FOREIGN KEY (voting_anchor_id) REFERENCES public.voting_anchor(id);


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE USAGE ON SCHEMA public FROM PUBLIC;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--
