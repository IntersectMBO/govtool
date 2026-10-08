// SPEC Appendix A: the query strings pdf-ui 1.0.18-beta builds, with sample
// values interpolated, exactly as sent (never URL-encoded). The unit corpus
// test parses each against its allowlist; e2e sends each unencoded.

export interface CorpusEntry {
  /** Route under /api, without the query. */
  route: string;
  /** Raw query string (no leading `?`). */
  query: string;
}

const TYPE_ID = 2;
const SEARCH = 'Demo';
const ID = 1;

const proposalsList = (sort: string, extra = '') =>
  `filters[$and][0][gov_action_type_id]=${TYPE_ID}&filters[$and][1][prop_name][$containsi]=${SEARCH}${extra}&pagination[page]=1&pagination[pageSize]=25&${sort}&populate[0]=proposal_links&populate[1]=proposal_withdrawals&populate[2]=proposal_constitution_content&populate[3]=proposal`;

export const PROPOSAL_SORTS = [
  'sort[createdAt]=DESC',
  'sort[createdAt]=ASC',
  'sort[proposal][prop_likes]=DESC',
  'sort[proposal][prop_likes]=ASC',
  'sort[proposal][prop_dislikes]=DESC',
  'sort[proposal][prop_dislikes]=ASC',
  'sort[proposal][prop_comments_number]=DESC',
  'sort[proposal][prop_comments_number]=ASC',
  'sort[prop_name]=ASC',
  'sort[prop_name]=DESC',
];

const commentsPopulate =
  'populate[comments_reports][populate][reporter][fields][0]=username&populate[comments_reports][populate][maintainer][fields][0]=username';

export const CORPUS: CorpusEntry[] = [
  // Proposals
  ...PROPOSAL_SORTS.map((s) => ({ route: 'proposals', query: proposalsList(s) })),
  {
    route: 'proposals',
    query: proposalsList('sort[createdAt]=DESC', '&filters[$and][2][prop_submitted]=true'),
  },
  {
    route: 'proposals',
    query: proposalsList('sort[createdAt]=DESC', '&filters[$and][2][prop_submitted]=false'),
  },
  {
    route: 'proposals',
    query:
      'filters[$and][2][is_draft]=true&pagination[page]=1&pagination[pageSize]=25&sort[createdAt]=desc&populate[0]=proposal_links&populate[1]=proposal_withdrawals&populate[2]=proposal_constitution_content',
  },
  {
    route: 'proposals',
    query:
      'filters[$and][2][is_draft]=true&filters[$and][3][prop_submitted]=false&pagination[page]=1&pagination[pageSize]=25&sort[createdAt]=desc&populate[0]=proposal_links&populate[1]=proposal_withdrawals&populate[2]=proposal_constitution_content',
  },
  { route: 'proposals', query: 'filters[$and][0][is_draft]=true&pagination[page]=1&pagination[pageSize]=1' },
  {
    route: 'proposals',
    query: `filters[$and][0][prop_id]=${ID}&pagination[page]=1&pagination[pageSize]=25&sort[createdAt]=desc&populate[0]=proposal_links&populate[1]=proposal_withdrawals`,
  },

  // Proposal votes, polls, poll votes
  { route: 'proposal-votes', query: `filters[proposal_id][$eq]=${ID}` },
  {
    route: 'polls',
    query: `filters[$and][0][proposal_id][$eq]=${ID}&filters[$and][1][is_poll_active]=true&pagination[page]=1&pagination[pageSize]=1&sort[createdAt]=desc`,
  },
  {
    route: 'polls',
    query: `filters[$and][0][proposal_id][$eq]=${ID}&filters[$and][1][is_poll_active]=false&pagination[page]=1&pagination[pageSize]=1&sort[createdAt]=desc`,
  },
  {
    route: 'poll-votes',
    query: `filters[poll_id][$eq]=${ID}&pagination[page]=1&pagination[pageSize]=1&sort[createdAt]=desc`,
  },

  // Comments
  ...['desc', 'asc'].flatMap((dir) => [
    {
      route: 'comments',
      query: `filters[$and][0][proposal_id]=${ID}&filters[$and][1][comment_parent_id][$null]=true&sort[createdAt]=${dir}&pagination[page]=1&pagination[pageSize]=25&${commentsPopulate}`,
    },
  ]),
  {
    route: 'comments',
    query: `filters[comment_parent_id]=${ID}&pagination[page]=1&pagination[pageSize]=3&sort[createdAt]=desc&${commentsPopulate}`,
  },
  {
    route: 'comments',
    query: `filters[comments_reports][hash][$eq]=${'a'.repeat(89)}&populate[comments_reports][populate][reporter]=*`,
  },

  { route: 'governance-action-types', query: '' },
];
