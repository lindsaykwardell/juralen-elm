import { defineCustomElement } from 'vue';

import ZoomButtons from './ZoomButtons.ce.vue'

const ZoomButtonsComponent = defineCustomElement(ZoomButtons);

customElements.define('zoom-buttons', ZoomButtonsComponent);