import 'event-source-polyfill';


// @flow
import 'event-source-polyfill';

export class SSE {

  eventSources : Map<string, EventSource>;

  constructor() {
    this.eventSources = new Map();
  }


  start(url, callback) {

    const eventSource = new EventSource(url);
    //when we remove the event source because we need to create a new one,
    //will these bindings still be retained and cause a memory leak
    eventSource.onopen = this.onopen.bind(this);
    eventSource.onerror = this.onerror.bind(this);

    eventSource.addEventListener("map_details", (data) => callback(data.data));

    this.eventSources.set(url, eventSource);
  }

  stop(url) {
    const eventSource = this.eventSources.delete(url);

    if (eventSource) {
      eventSource.close();
    }
  }

  stopAll() {
    Array.from(this.eventSources.values()).forEach(source => source.close());
    this.eventSources.clear();
  }


  onopen(message) {

  }

  onerror(message) {

  }
}
