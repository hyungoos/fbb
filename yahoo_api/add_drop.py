import argparse
import os

from yahoo_oauth import OAuth2

ADD_BODY = """
<fantasy_content>
  <transaction>
    <type>add</type>
    <player>
      <player_key>mlb.p.%d</player_key>
      <transaction_data>
        <type>add</type>
        <destination_team_key>mlb.l.%d.t.%d</destination_team_key>
      </transaction_data>
    </player>
  </transaction>
</fantasy_content>
"""

DROP_BODY = """
<fantasy_content>
  <transaction>
    <type>drop</type>
    <player>
      <player_key>mlb.p.%d</player_key>
      <transaction_data>
        <type>drop</type>
        <source_team_key>mlb.l.%d.t.%d</source_team_key>
      </transaction_data>
    </player>
  </transaction>
</fantasy_content>
"""

ADD_DROP_BODY = """
<fantasy_content>
  <transaction>
    <type>add/drop</type>
    <players>
      <player>
        <player_key>mlb.p.%d</player_key>
        <transaction_data>
          <type>add</type>
          <destination_team_key>mlb.l.%d.t.%d</destination_team_key>
        </transaction_data>
      </player>
      <player>
        <player_key>mlb.p.%d</player_key>
        <transaction_data>
          <type>drop</type>
          <source_team_key>mlb.l.%d.t.%d</source_team_key>
        </transaction_data>
      </player>
    </players>
  </transaction>
</fantasy_content>
"""

def main(args):
	oauth = OAuth2(None, None, from_file=args.token_path)
	if not oauth.token_is_valid():
	    oauth.refresh_access_token()

	url = "https://fantasysports.yahooapis.com/fantasy/v2/league/mlb.l.%d/transactions" % args.league_id

	if args.add_id and args.drop_id:
		body = ADD_DROP_BODY % (args.add_id, args.league_id, args.team_id, args.drop_id, args.league_id, args.team_id)
	elif args.drop_id:
		body = DROP_BODY % (args.drop_id, args.league_id, args.team_id)
	elif args.add_id:
		body = ADD_BODY % (args.add_id, args.league_id, args.team_id)

	resp = oauth.session.post(url, data=body, headers = {'Content-Type': 'application/xml'})
	print "response: " + resp._content

def build_parser():
    parser = argparse.ArgumentParser(description='Add/Drop a player from Yahoo FBB')
    parser.add_argument(
        "--token-path",
        dest="token_path",
        default=os.path.expanduser("~") + '/yahoo_token.json',
        help="json token file")

    # TODO: Take a list of player ids and try them in order until we run out of players to drop
    parser.add_argument(
        "--add",
        dest="add_id",
        default=None,
        type=int,
        help="player id to add")

    parser.add_argument(
        "--drop",
        dest="drop_id",
        default=None,
        type=int,
        help="player id to drop")

    parser.add_argument(
        "--league",
        dest="league_id",
        default=19797,
        type=int,
        help="league id")

    parser.add_argument(
        "--team",
        dest="team_id",
        default=4,
        type=int,
        help="team id")

    return parser


if __name__ == '__main__':
    args = build_parser().parse_args()
    main(args)
