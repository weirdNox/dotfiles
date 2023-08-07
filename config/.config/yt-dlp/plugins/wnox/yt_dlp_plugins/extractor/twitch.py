from yt_dlp.extractor.twitch import TwitchVodIE

from yt_dlp.compat import compat_parse_qs, compat_urllib_parse_urlencode, compat_urllib_parse_urlparse
from yt_dlp.utils import update_url_query

import re

class _TwitchVodPluginIE(TwitchVodIE, plugin_name="override"):
    def _real_extract(self, url):
        vod_id = self._match_id(url)

        video = self._download_info(vod_id)
        info = self._extract_info_gql(video, vod_id)
        access_token = self._download_access_token(vod_id, "video", "id")
        restricted_bitrates = self._parse_json(access_token["value"], None).get("chansub", {}).get("restricted_bitrates")

        formats = list(self._extract_storyboard(vod_id, video.get("storyboard"), info.get("duration")))

        if len(restricted_bitrates) == 0:
            formats.extend(self._extract_m3u8_formats(
                "%s/vod/%s.m3u8?%s" % (
                    self._USHER_BASE, vod_id,
                    compat_urllib_parse_urlencode({
                        "allow_source": "true",
                        "allow_audio_only": "true",
                        "allow_spectre": "true",
                        "player": "twitchweb",
                        "playlist_include_framerate": "true",
                        "nauth": access_token["value"],
                        "nauthsig": access_token["signature"],
                    })),
                vod_id, "mp4", entry_protocol="m3u8_native"))
        else:
            storyboard_url = formats[0]["url"]
            base_url = storyboard_url[:storyboard_url.index("/storyboards/")]
            for quality in restricted_bitrates:
                quality_info = {
                    "format_id": quality,
                    "url": f"{base_url}/{quality}/index-dvr.m3u8",
                }
                if quality == "audio_only": quality_info["vcodec"] = "none"
                elif quality == "chunked":  quality_info["height"] = 1080
                else:
                    m = re.match(r"(\d+)p(\d+)?", quality)
                    if m:
                        quality_info["height"] = int(m.group(1))
                        if m.group(2): quality_info["fps"] = int(m.group(2))
                formats.append(quality_info)

        self._prefer_source(formats)
        info["formats"] = formats

        parsed_url = compat_urllib_parse_urlparse(url)
        query = compat_parse_qs(parsed_url.query)
        if "t" in query:
            info["start_time"] = parse_duration(query["t"][0])

        return info
