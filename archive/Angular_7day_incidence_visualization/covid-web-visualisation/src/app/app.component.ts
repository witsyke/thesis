import { Component, OnInit } from '@angular/core';
import * as d3 from 'd3v4';
import * as data from '../assets/rki_geo_2.json';
import * as dates from '../assets/dates.json';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css'],
})
export class AppComponent implements OnInit {
  title = 'covid-web-visualisation';

  thresholds = [0.01, 5, 25, 50, 100, 250, 500];

  color_legend = d3
    .scaleThreshold<string>()
    .range([
      '#C4CCF5',
      '#FAF7C9',
      '#FFEE7D',
      '#FAB133',
      '#D03523',
      '#921214',
      '#651212',
      '#D80182',
    ])
    .domain(this.thresholds);

  dataset = data['default'];
  dateArray = dates['default'];
  currentDate = '18.11.2020';

  ngOnInit(): void {
    this.setMap(1000, 600, this.currentDate);
    this.createLegend();
    this.transitionMap();

    // let time = 1;
    // let interval = setInterval(() => {
    //   if (time <= this.dateArray.length - 1) {
    //     this.currentDate = this.dateArray[time];
    //     this.transitionMap(this.currentDate);
    //     time++;
    //   } else {
    //     time = 0;
    //     // clearInterval(interval);
    //   }
    // }, 2000);
  }

  setMap(width: number, height: number, date: string) {
    const margin = { top: 10, right: 30, bottom: 10, left: 30 };

    width = width - margin.left - margin.right;
    height = height - margin.top - margin.bottom;
    const projection = d3.geoMercator().scale(1).translate([0, 0]);

    const path = d3.geoPath().projection(projection);
    const svg = d3
      .select('.world-map')
      .append('svg')
      .attr('viewBox', '0 0 1000 600')
      .attr('preserveAspectRatio', 'xMidYMid')
      .style('max-width', 1200)
      .style('margin', 'auto')
      .style('display', 'flex');
    const b = path.bounds(this.dataset),
      s =
        0.95 /
        Math.max((b[1][0] - b[0][0]) / width, (b[1][1] - b[0][1]) / height),
      t = [
        (width - s * (b[1][0] + b[0][0])) / 2,
        (height - s * (b[1][1] + b[0][1])) / 2,
      ];
    projection.scale(s).translate(t);
    svg
      .selectAll('path')
      .data(this.dataset.features)
      .enter()
      .append('path')
      .attr('d', path)
      .style('fill', (d) => {
        const value = d.properties[date];
        if (value !== undefined) {
          return this.color_legend(value);
        } else {
          return '#ccc';
        }
      })
      .style('stroke', '#fff')
      .style('stroke-width', '0.5')
      .text((d) => d.properties[date]);
  }

  transitionMap() {
    const svg = d3.select('.world-map');
    let title = d3.select('#date');

    let test = svg.selectAll('path').data(this.dataset.features);

    this.dateArray.forEach((date) => {
      title = title
        .transition()
        .delay(5000)
        .duration(250)
        .text(() => date);
      test = test
        .transition()
        // .delay(500)
        .duration(750)
        .style('fill', (d) => {
          const value = d.properties[date];
          if (value !== undefined) {
            return this.color_legend(value);
          } else {
            return '#ccc';
          }
        });
    });
  }

  createLegend() {
    const svg = d3.select('.world-map svg');
    svg
      .selectAll('mydots')
      .data(this.thresholds)
      .enter()
      .append('circle')
      .attr('cx', 100)
      .attr('cy', (_, i) => 100 + i * 25) // 100 is where the first dot appears. 25 is the distance between dots
      .attr('r', 7)
      .style('fill', (d) => this.color_legend(d));

    svg
      .append('circle')
      .attr('cx', 100)
      .attr('cy', (_, i) => 75) // 100 is where the first dot appears. 25 is the distance between dots
      .attr('r', 7)
      .style('fill', (d) => '#ccc');

    // Add one dot in the legend for each name.
    svg
      .selectAll('mylabels')
      .data(this.thresholds)
      .enter()
      .append('text')
      .attr('x', 120)
      .attr('y', (_, i) => 100 + i * 25) // 100 is where the first dot appears. 25 is the distance between dots
      // .style('fill', (d) => this.color_legend(d))
      .style('fill', 'white')
      .text((d) => (d == 0.01 ? 0 : d))
      .attr('text-anchor', 'left')
      .style('alignment-baseline', 'middle');

    svg
      .append('text')
      .attr('x', 120)
      .attr('y', (_, i) => 75) // 100 is where the first dot appears. 25 is the distance between dots
      // .style('fill', (d) => this.color_legend(d))
      .style('fill', 'white')
      .text((d) => 'No data available')
      .attr('text-anchor', 'left')
      .style('alignment-baseline', 'middle');
  }

  calculateDaysBetweenDates() 
}
